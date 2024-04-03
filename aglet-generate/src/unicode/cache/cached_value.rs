use std::error::Error;
use std::fmt;
use std::future::Future;
use std::marker::PhantomData;
use std::ops::Deref;

use chrono::{DateTime, Utc};
use serde::de::Visitor;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[derive(Debug)]
pub enum CachedValue<T> {
    Cached(CachedData<T>),
    None,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CachedData<T> {
    #[serde(with = "toml_datetime_compat")]
    pub expires_at: DateTime<Utc>,
    pub value: T,
}

impl<T> CachedValue<T> {
    pub async fn get_or_replace<F, E>(
        &mut self,
        ttl: chrono::Duration,
        replacer: F,
    ) -> Result<&T, E>
    where
        F: Future<Output = Result<T, E>>,
    {
        if !self.is_valid() {
            *self = Self::Cached(CachedData {
                value: replacer.await?,
                expires_at: Utc::now() + ttl,
            });
        }

        Ok(self.valid().unwrap())
    }

    pub fn valid(&self) -> Option<&T> {
        match self {
            Self::Cached(data) if data.expires_at > Utc::now() => Some(&data.value),
            _ => None,
        }
    }

    pub fn is_valid(&self) -> bool {
        matches!(self, Self::Cached(data) if data.expires_at > Utc::now())
    }

    pub fn is_expired(&self) -> bool {
        matches!(self, Self::Cached(data) if data.expires_at <= Utc::now())
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

impl<T> Default for CachedValue<T> {
    fn default() -> Self {
        Self::None
    }
}

impl<T> Serialize for CachedValue<T>
where
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Cached(data) => serializer.serialize_some(data),
            Self::None => serializer.serialize_none(),
        }
    }
}

struct CachedValueVisitor<T> {
    marker: PhantomData<T>,
}

impl<'de, T> Visitor<'de> for CachedValueVisitor<T>
where
    T: Deserialize<'de>,
{
    type Value = CachedValue<T>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("cached value")
    }

    #[inline]
    fn visit_none<E>(self) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(Self::Value::None)
    }

    #[inline]
    fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        CachedData::deserialize(deserializer).map(Self::Value::Cached)
    }

    #[inline]
    fn visit_unit<E>(self) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(Self::Value::None)
    }
}

impl<'de, T> Deserialize<'de> for CachedValue<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_option(CachedValueVisitor {
            marker: PhantomData,
        })
    }
}

impl<T> Deref for CachedData<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

pub trait Cacheable {
    type Target;

    fn cached(self, expires_at: DateTime<Utc>) -> CachedValue<Self::Target>;
}

impl<T> Cacheable for T {
    type Target = T;

    fn cached(self, expires_at: DateTime<Utc>) -> CachedValue<Self::Target> {
        CachedValue::Cached(CachedData {
            value: self,
            expires_at,
        })
    }
}
