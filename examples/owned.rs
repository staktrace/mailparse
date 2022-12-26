extern crate mailparse;
extern crate ouroboros;

use mailparse::{parse_mail, ParsedMail};
use ouroboros::self_referencing;

#[self_referencing]
struct OwnedMail {
    raw_bytes: Vec<u8>,
    #[borrows(raw_bytes)]
    #[covariant]
    parsed: ParsedMail<'this>,
}

fn make_owned_mail(mail_bytes: Vec<u8>) -> OwnedMail {
    OwnedMailBuilder {
        raw_bytes: mail_bytes,
        parsed_builder: |b: &Vec<u8>| parse_mail(b).unwrap(),
    }
    .build()
}

fn main() {
    let owned = make_owned_mail(b"Key: value\r\n\r\nSome body stuffs".to_vec());
    println!(
        "Mail body is: {}",
        owned.borrow_parsed().get_body().unwrap()
    );
}
