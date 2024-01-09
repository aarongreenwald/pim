import {format} from 'date-fns';
import {formatInTimeZone} from 'date-fns-tz';
import {BasicISODate} from '../../../common/financials.models';

// Given a date in the expanded ISO8601 format: `2023-11-24`,
// collapse to the basic format: 20231124 used in  the DB. 
export function collapseISODate(date: string) : BasicISODate | string {

  // This early exit hack exists because of the datepicker.
  //for dates like '01/05/2021' user could type '1' and then TAB to get to the next field
  //but because of the way the date is displayed it seems natural to type `01`. When the 0 is entered, the datepicker
  //sends an empty string as the date, so we bail - when the user types the `1` we'll get back to this function
  //with a proper date string
  
  if (date.length != 10 || date[0] === '0') {
    return date
  }

  // This can also be accomplished more elegantly using the Date object and/or libraries, but I like this approach
  // because if used inorrectly, it will fail spectacularly instead of hiding subtle bugs that only appear when the user
  // is in a particular timezone.
  return parseInt(date.split('-').join('')) 
}

// Given a date in the basic ISO format: 20231124, expand to
// the ISO format `2023-11-24`. Useful because JS Date doesn't
// understand the basic format. 
export function expandISODate(date: number): string {  
  const str = date.toString()
  if (str.length != 8) return str // early exit - see comment about datepicker in collapseISODate, this is the other half of the round trip
  return `${str.slice(0,4)}-${str.slice(4,6)}-${str.slice(6,8)}`
}

// Expands 20231124 to the date format used in list displays. 
export function isoDateToFullDisplay(date: number) : string {
  const expanded = expandISODate(date)
  if (expanded.length != 10) return date.toString() // shouldn't happen unless we have bad data in the DB
  return expanded
  // TODO this isn't staying in UTC, for some reason? I'd like a better format than expanded iso
  //it only seems ok because I am always ahead of UTC, so the day is the same. Set the client clock
  //to less than UTC and days will all be off by one.
  // return formatInTimeZone(expanded, 'UTC', 'yyyy-MM-dd iii')  
}

export function todayAsISODate() : BasicISODate {
  return collapseISODate(formatInTimeZone(new Date(), 'UTC', 'yyyy-MM-dd')) as BasicISODate;
}

// Deprecated. Avoid using!
export function formatDay(date: number | Date | string) { 
    try {
      return formatInTimeZone(new Date(date), 'UTC', 'yyyy-MM-dd');
    } catch {
        //for dates like '01/05/2021' user could type '1' and then TAB to get to the next field
        //but because of the way the date is displayed it seems natural to type `01`. When the 0 is entered,
        //this method will throw on the new Date(), so just return the input until the next character is entered
        //Note: a side benefit of typing 01 is that no tab is needed to get to the next field.
        return date as string;
    }

}

export function formatTime(date: number | Date | string) {
    return format(new Date(date), 'h:mm bbb');
}

export function formatTimeInput(date: number | Date | string) {
    return format(new Date(date), 'HH:mm:ss.sss');
}

export function isMidnight(date: number | Date | string) {
    const asDate = new Date(date)
    return asDate.getUTCHours() === 0 && asDate.getUTCMinutes() === 0 //that's enough to safely assume it's midnight.
}
