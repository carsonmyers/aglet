use async_ftp::FtpStream;
use deadpool::managed;
use deadpool::managed::{Metrics, RecycleError, RecycleResult};

use super::ftp::login;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Connection error: {0}")]
    ConnectionError(eyre::Report),
}

pub struct Manager;

impl managed::Manager for Manager {
    type Type = FtpStream;
    type Error = Error;

    async fn create(&self) -> Result<Self::Type, Self::Error> {
        login().await.map_err(Error::ConnectionError)
    }

    async fn recycle(
        &self,
        obj: &mut Self::Type,
        _metrics: &Metrics,
    ) -> RecycleResult<Self::Error> {
        obj.noop()
            .await
            .map_err(|err| RecycleError::message(format!("{}", err)))
    }
}

pub type Pool = managed::Pool<Manager>;

pub fn new_pool(max_connections: usize) -> Pool {
    Pool::builder(Manager)
        .max_size(max_connections)
        .build()
        .expect("ftp connection pool")
}
