import * as React from 'react';
import {DropdownItemDto} from '@pim/common';
import {useMemo, useEffect} from 'react';
import {ComboBox} from '@fluentui/react';

interface DropdownProps {
  items: DropdownItemDto[];
  value: number;
  onChange: (id: number) => void;
  label?: string;
  placeholder?: string;
}

export const Dropdown: React.FC<DropdownProps> = ({items, value, onChange, label, placeholder}) => {
  

  const defaultValue = useMemo(() => items?.find(item => item.default)?.id, [items])
  useEffect(() => {
    if (!value && defaultValue) onChange(defaultValue)
  }, [value, defaultValue, onChange])

  const options = useMemo(() => items?.map(item => ({
    key: item.id,
    text: item.name,
    data: item
  })), [items]);

  return (
    <ComboBox
      autoComplete="on"
      placeholder={placeholder}
      label={label}
      selectedKey={value}
      onChange={(ev, option) => onChange(option.key as unknown as number)}
      onRenderOption={(option) => <>{option.data.name}</>}
      options={options}/>
  )
}
