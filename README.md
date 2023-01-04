mailparse
===
![Build Status](https://github.com/staktrace/mailparse/actions/workflows/test.yml/badge.svg)
[![Crate](https://img.shields.io/crates/v/mailparse.svg)](https://crates.io/crates/mailparse)

A simple parser for MIME email messages.

API
---
The primary entry point for this library is the following function:

```rust
    parse_mail(&[u8]) -> Result<ParsedMail, MailParseError>
```

This function takes the raw message data, including headers and body, and returns a structured object to more easily access pieces of the email message.
There are other public functions that allow parsing smaller parts of the message as well; refer to the [full documentation](https://docs.rs/mailparse/).

The library is designed to process real-world email data such as might be obtained by using the FETCH command on an IMAP server, or in a Maildir.
As such, this library should successfully handle any valid MIME-formatted message, although it may not follow all the strict requirements in the various specifications that cover the format (predominantly IETF RFCs 822, 2045, 2047, 2822, and 5322).
As an example, this library accepts raw message data which uses \n (ASCII LF) as line delimiters rather than the RFC-mandated \r\n (ASCII CRLF) line delimiters.

Example usage
---

```rust
    use mailparse::*;
    let parsed = parse_mail(concat!(
            "Subject: This is a test email\n",
            "Content-Type: multipart/alternative; boundary=foobar\n",
            "Date: Sun, 02 Oct 2016 07:06:22 -0700 (PDT)\n",
            "\n",
            "--foobar\n",
            "Content-Type: text/plain; charset=utf-8\n",
            "Content-Transfer-Encoding: quoted-printable\n",
            "\n",
            "This is the plaintext version, in utf-8. Proof by Euro: =E2=82=AC\n",
            "--foobar\n",
            "Content-Type: text/html\n",
            "Content-Transfer-Encoding: base64\n",
            "\n",
            "PGh0bWw+PGJvZHk+VGhpcyBpcyB0aGUgPGI+SFRNTDwvYj4gdmVyc2lvbiwgaW4g \n",
            "dXMtYXNjaWkuIFByb29mIGJ5IEV1cm86ICZldXJvOzwvYm9keT48L2h0bWw+Cg== \n",
            "--foobar--\n",
            "After the final boundary stuff gets ignored.\n").as_bytes())
        .unwrap();
    assert_eq!(parsed.headers.get_first_value("Subject"),
        Some("This is a test email".to_string()));
    assert_eq!(parsed.subparts.len(), 2);
    assert_eq!(parsed.subparts[0].get_body().unwrap(),
        "This is the plaintext version, in utf-8. Proof by Euro: \u{20AC}");
    assert_eq!(parsed.subparts[1].headers[1].get_value(), "base64");
    assert_eq!(parsed.subparts[1].ctype.mimetype, "text/html");
    assert!(parsed.subparts[1].get_body().unwrap().starts_with("<html>"));
    assert_eq!(dateparse(parsed.headers.get_first_value("Date").unwrap().as_str()).unwrap(), 1475417182);
```

Documentation
---
See the rustdoc at [docs.rs](https://docs.rs/mailparse/).

MSRV policy
---
Currently the minimum supported Rust version (MSRV) is 1.51.0.
MSRV increases will be kept to a minimum, and will always be accompanied with a minor version bump.

Support mailparse
---
If you want to support development of `mailparse`, please do so by donating your money, time, and/or energy to fighting climate change.
A quick and easy way is to send a donation to [Replant.ca Environmental](http://www.replant-environmental.ca/donate.html), where every dollar gets a tree planted!
