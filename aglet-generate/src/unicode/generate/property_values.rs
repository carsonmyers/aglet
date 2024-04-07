use std::collections::HashSet;

pub struct PropertyValue {
    pub property_name: String,
    pub aliases:       HashSet<String>,
}

impl PropertyValue {
    pub fn new<T, U>(property_name: U, aliases: T) -> Self
    where
        T: AsRef<[U]>,
        U: AsRef<str>,
    {
        let mut set = HashSet::new();
        for alias in aliases.as_ref() {
            set.insert(alias.as_ref().to_string());
        }

        Self {
            property_name: property_name.as_ref().to_string(),
            aliases:       set,
        }
    }
}
