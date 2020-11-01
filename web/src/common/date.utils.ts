import {format} from 'date-fns';

export function formatDay(date: number | Date | string) {
    return format(new Date(date), 'yyyy-MM-dd');
}
