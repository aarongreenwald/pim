import {useCallback, useState} from 'react';
import _debounce from 'lodash/debounce';

export function useDebouncedInput<T = string>(initialValue: T) {
    const [inputVal, setInputVal] = useState<T>(initialValue);
    const [debouncedValue, setDebouncedValue] = useState(initialValue);
    const updateDebouncedValue = useCallback(_debounce(setDebouncedValue, 300), [setDebouncedValue]) //eslint-disable-line react-hooks/exhaustive-deps
    const updateValue = useCallback(val => {
        setInputVal(val);
        updateDebouncedValue(val);
    }, [setInputVal, updateDebouncedValue]);
    return {inputVal, debouncedValue, updateValue};
}
