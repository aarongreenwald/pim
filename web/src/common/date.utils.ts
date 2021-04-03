import {format} from 'date-fns';

export function formatDay(date: number | Date | string) {
    return format(new Date(date), 'yyyy-MM-dd');
}

export function formatTime(date: number | Date | string) {
    return format(new Date(date), 'h:mm bbb');
}

export function isMidnight(date: number | Date | string) {
    const asDate = new Date(date)
    return asDate.getUTCHours() === 0 && asDate.getUTCMinutes() === 0 //that's enough to safely assume it's midnight.
}