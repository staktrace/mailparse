extern crate mailparse;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use mailparse::MailHeaderMap;

fn dump(pfx: &str, pm: &mailparse::ParsedMail) {
    println!(">> Headers from {} <<", pfx);
    for h in &pm.headers {
        println!(
            "  [{}] => [{}]",
            h.get_key().unwrap(),
            h.get_value().unwrap()
        );
    }

    println!(">> Addresses from {} <<", pfx);
    for address_header in ["From", "To", "Cc", "Bcc"] {
        if let Some(Ok(addresses)) = pm.headers.get_first_value(address_header) {
            println!("{:?}", mailparse::addrparse(&addresses).unwrap());
        }
    }

    println!(">> Body from {} <<", pfx);
    if pm.ctype.mimetype.starts_with("text/") {
        println!("  [{}]", pm.get_body().unwrap());
    } else {
        println!(
            "   (Body is binary type {}, {} bytes in length)",
            pm.ctype.mimetype,
            pm.get_body().unwrap().len()
        );
    }

    let mut c = 1;
    for s in &pm.subparts {
        println!(">> Subpart {} <<", c);
        dump("subpart", s);
        c += 1;
    }
}

// Provide mail files as arguments
fn main() {
    let mut args = env::args();
    args.next();
    loop {
        match args.next() {
            None => break,
            Some(a) => {
                let mut f = File::open(&a).unwrap();
                let mut d = Vec::<u8>::new();
                f.read_to_end(&mut d).unwrap();
                let mail = mailparse::parse_mail(&d).unwrap();
                dump(&a, &mail);
            }
        }
    }
}
