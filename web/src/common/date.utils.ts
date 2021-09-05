import {format} from 'date-fns';

export function formatDay(date: number | Date | string) {
    try {
        return format(new Date(date), 'yyyy-MM-dd');
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

export function isMidnight(date: number | Date | string) {
    const asDate = new Date(date)
    return asDate.getUTCHours() === 0 && asDate.getUTCMinutes() === 0 //that's enough to safely assume it's midnight.
}