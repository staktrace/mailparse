#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate mailparse;

use mailparse::*;

fuzz_target!(|data: &[u8]| {
    if let Ok(parsed) = parse_mail(data) {
        if let Some(date) = parsed.headers.get_first_value("Date") {
            let _ = dateparse(&date);
        }
    }
});
