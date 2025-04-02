#[macro_export]
macro_rules! rc_cell {
    ($value:expr) => {
        ::std::rc::Rc::new(::std::cell::RefCell::new($value))
    };
}
