import {useCallback, useState} from 'react';
import _debounce from 'lodash/debounce';

export function useDebouncedInput() {
    const [inputVal, setInputVal] = useState('');
    const [debouncedValue, setDebouncedValue] = useState('');
    const updateDebouncedValue = useCallback(_debounce(setDebouncedValue, 300), [setDebouncedValue])
    const updateValue = useCallback(val => {
        setInputVal(val);
        updateDebouncedValue(val);
    }, [setInputVal, updateDebouncedValue]);
    return {inputVal, debouncedValue, updateValue};
}