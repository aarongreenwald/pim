import * as React from 'react';
import styled from '@emotion/styled';
import {useCategories} from './categories';
import {CategoryId} from '@pim/common';
import {useCallback} from 'react';

interface CategoryDropdownProps {
    value: CategoryId;
    onChange: (id: CategoryId) => void;
    name?: string;
}

export const CategoryDropdown: React.FC<CategoryDropdownProps> = ({value, onChange, name}) => {
    const categories = useCategories();
    const onChangeCallback = useCallback((event: React.ChangeEvent<HTMLSelectElement>) =>
        onChange(Number(event.target.value)), [onChange])
    return (
        <StyledSelect
            name={name}
            value={value || -1}
            onChange={onChangeCallback}>
            <option disabled value={-1}> Select category </option>
            {
                categories && categories.map(category =>
                    <option
                        key={category.id}
                        value={category.id}>
                        {category.name}
                    </option>)
            }
        </StyledSelect>
    )

}

const StyledSelect = styled.select`
  width: 100%;
`
