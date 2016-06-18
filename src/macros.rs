macro_rules! try_none {
    ( $x:expr ) => {{
        match $x {
            Some(v) => v,
            None => return None,
        }
    }}
}
