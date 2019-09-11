use ::std::collections::HashMap;
use ::std::hash::Hash;
use ::std::ops::Index;
use ::std::fmt::Debug;

pub struct HashMapStack<K, V> {
    stack: Vec<HashMap<K, V>>,
}

impl<K, V> HashMapStack<K, V> where K: Hash + Eq {

    pub fn new() -> Self {
        HashMapStack {
            stack: vec![],
        }
    }

    pub fn push(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        if self.stack.len() == 0 {
            panic!("attempt too pop from empty HashMapStack");
        }

        self.stack.pop();
    }

    pub fn layer(&self, n: usize) -> &HashMap<K, V> {
        &self.stack[n]
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.stack.last_mut()
            .expect("attempt to insert into empty HashMapStack")
            .insert(key, value);
    }

    pub fn get<'a>(&self, key: &K) -> Option<&V> {
        self.resolve_first(|l| l.get(key))
    }

    pub fn contains_key(&self, key: &K) -> bool {
        let resolved = self.resolve_first(|l| {
            match l.contains_key(key) {
                true => Some(()),
                false => None,
            }
        });
        resolved == Some(())
    }

    pub fn height(&self) -> usize {
        self.stack.len()
    }

    pub fn resolve_first<'a, F, T>(&'a self, resolver: F) -> Option<T>
        where F: Fn(&'a HashMap<K, V>) -> Option<T>, T: 'a {

        for level in self.stack.iter().rev() {
            if let Some(inner) = resolver(level) {
                return Some(inner);
            }
        }
        None
    }

    pub fn flatten(&self) -> HashMap<&K, &V> {
        let mut out = HashMap::new();

        for level in self.stack.iter() {
            for (key, value) in level {
                out.insert(key, value);
            }
        }

        out
    }

}

impl<K, V> HashMapStack<K, V> where K: Hash + Eq + Clone, V: Copy {

    pub fn flatten_clone(&self) -> HashMap<K, V> {
        let mut out = HashMap::new();

        for level in self.stack.iter() {
            for (key, value) in level {
                out.insert(key.clone(), value.clone());
            }
        }

        out
    }

}

impl<'a, K, V> Index<&'a K> for HashMapStack<K, V>
    where K: Eq + Hash {
    type Output = V;

    fn index(&self, index: &K) -> &V {
        self.get(index).expect("no entry found for key")
    }
}

impl<K, V> Debug for HashMapStack<K, V> where K: Debug + Hash + Eq, V: Debug {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        let flat = self.flatten();
        write!(f, "{:?}", flat)
    }
}

#[cfg(test)]
mod test {
    use super::HashMapStack;

    #[test]
    fn simple() {
        let mut hms = HashMapStack::new();

        assert!(!hms.contains_key(&0));

        hms.push();
        hms.insert(0u32, 0u32);
        assert!(hms.contains_key(&0));

        hms.push();
        hms.insert(1, 0);
        assert!(hms.contains_key(&0));
        assert!(hms[&0] == 0);
        hms.insert(0, 1);
        assert!(hms[&0] == 1);
        assert!(hms[&1] == 0);

        hms.pop();
        assert!(hms[&0] == 0);
        assert!(!hms.contains_key(&1));

        hms.pop();
        assert!(!hms.contains_key(&0));
    }

    #[test]
    #[should_panic]
    fn insert_in_empty() {
        let mut hms = HashMapStack::new();
        hms.insert(0u32, 0u32);
    }

    #[test]
    fn flatten() {
        let mut hms = HashMapStack::new();

        hms.push();
        hms.insert(0u32, 0u32);

        hms.push();
        hms.insert(1, 0);

        let flat = hms.flatten_clone();
        assert!(flat.contains_key(&0));
        assert!(flat.contains_key(&1));
    }

}
