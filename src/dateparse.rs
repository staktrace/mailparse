#[derive(PartialEq, Debug)]
pub struct DateTime {
    /// The year, e.g. 2016
    pub year: u32,
    /// The month (0 is January and 11 is December).
    pub month: i8,
    /// The date (one-indexed, so 31 would indicate the last date in January).
    pub date: i8,
    /// The hour (zero-indexed, valid values are 0 to 23 inclusive)
    pub hour: i8,
    /// The minute (zero-indexed, valid values are 0 to 59 inclusive)
    pub minute: i8,
    /// The second (zero-indexed, valid values are 0 to 59 inclusive)
    pub second: i8,
}

enum DateParseState {
    Date,
    Month,
    Year,
    Hour,
    Minute,
    Second,
    Timezone,
}

fn days_in_month(month: i8, year: u32) -> i8 {
    match month {
        0 | 2 | 4 | 6 | 7 | 9 | 11 => 31,
        3 | 5 | 8 | 10 => 30,
        1 => {
            if (year % 400) == 0 {
                29
            } else if (year % 100) == 0 {
                28
            } else if (year % 4) == 0 {
                29
            } else {
                28
            }
        },
        _ => 0,
    }
}

pub fn dateparse(date: &str) -> Result<DateTime, &'static str> {
    let mut result = DateTime { year: 0, month: 0, date: 1, hour: 0, minute: 0, second: 0 };
    let mut state = DateParseState::Date;
    for tok in date.split(|c| c == ' ' || c == ':') {
        if tok.is_empty() {
            continue;
        }
        match state {
            DateParseState::Date => {
                match tok.parse::<i8>() {
                    Ok(v) => {
                        result.date = v;
                        state = DateParseState::Month;
                    },
                    Err(_) => (),
                };
                continue;
            },
            DateParseState::Month => {
                result.month = match tok.to_uppercase().as_str() {
                    "JAN" | "JANUARY" => 0,
                    "FEB" | "FEBRUARY" => 1,
                    "MAR" | "MARCH" => 2,
                    "APR" | "APRIL" => 3,
                    "MAY" => 4,
                    "JUN" | "JUNE" => 5,
                    "JUL" | "JULY" => 6,
                    "AUG" | "AUGUST" => 7,
                    "SEP" | "SEPTEMBER" => 8,
                    "OCT" | "OCTOBER" => 9,
                    "NOV" | "NOVEMBER" => 10,
                    "DEC" | "DECEMBER" => 11,
                    _ => return Err("Unrecognized month"),
                };
                state = DateParseState::Year;
                continue;
            },
            DateParseState::Year => {
                result.year = match tok.parse::<u32>() {
                    Ok(v) if v < 70 => 2000 + v,
                    Ok(v) if v < 100 => 1900 + v,
                    Ok(v) => v,
                    Err(_) => return Err("Invalid year"),
                };
                state = DateParseState::Hour;
                continue;
            },
            DateParseState::Hour => {
                result.hour = match tok.parse::<i8>() {
                    Ok(v) => v,
                    Err(_) => return Err("Invalid hour"),
                };
                state = DateParseState::Minute;
                continue;
            },
            DateParseState::Minute => {
                result.minute = match tok.parse::<i8>() {
                    Ok(v) => v,
                    Err(_) => return Err("Invalid minute"),
                };
                state = DateParseState::Second;
                continue;
            },
            DateParseState::Second => {
                result.second = match tok.parse::<i8>() {
                    Ok(v) => v,
                    Err(_) => return Err("Invalid second"),
                };
                state = DateParseState::Timezone;
                continue;
            },
            DateParseState::Timezone => {
                let (tz, tz_sign) = match tok.parse::<i16>() {
                    Ok(v) if v < 0 => (-v, -1),
                    Ok(v) => (v, 1),
                    Err(_) => return Err("Invalid timezone"),
                };
                let tz_hours = (tz / 100) as i8;
                let tz_mins = (tz % 100) as i8;
                if tz_sign < 0 {
                    result.hour += tz_hours;
                    result.minute += tz_mins;
                } else {
                    result.hour -= tz_hours;
                    result.minute -= tz_mins;
                }
                while result.minute < 0 {
                    result.hour -= 1;
                    result.minute += 60;
                }
                while result.minute >= 60 {
                    result.hour += 1;
                    result.minute -= 60;
                }
                while result.hour < 0 {
                    result.date -= 1;
                    result.hour += 24;
                }
                while result.hour >= 24 {
                    result.date += 1;
                    result.hour -= 24;
                }
                let num_days = days_in_month(result.month % 12, result.year);
                if result.date < 0 || result.date > num_days + 1 {
                    return Err("Invalid date after normalization");
                }
                if result.date < 1 {
                    result.month -= 1;
                    result.date = days_in_month(result.month % 12, result.year);
                } else if result.date > num_days {
                    result.month += 1;
                    result.date = 1;
                }
                while result.month < 0 {
                    result.year -= 1;
                    result.month += 12;
                }
                while result.month >= 12 {
                    result.year += 1;
                    result.month -= 12;
                }
                break;
            },
        }
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_dates() {
        assert_eq!(dateparse("Sun, 25 Sep 2016 18:36:33 -0400").unwrap(),
                   DateTime{ year: 2016, month: 8, date: 25, hour: 22, minute: 36, second: 33 });
    }
}
