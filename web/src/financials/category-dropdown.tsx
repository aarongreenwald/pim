import * as React from 'react';
import {useCategories} from './categories';
import {CategoryId} from '@pim/common';
import {useMemo} from 'react';
import {ComboBox} from '@fluentui/react';

interface CategoryDropdownProps {
    value: CategoryId;
    onChange: (id: CategoryId) => void;
}

export const CategoryDropdown: React.FC<CategoryDropdownProps> = ({value, onChange}) => {
    const categories = useCategories();
    const options = useMemo(() => categories?.map(category => ({
        key: category.id,
        text: category.name,
        data: category
    })), [categories]);

    return (
        <ComboBox
            autoComplete="on"
            placeholder="Select category"
            selectedKey={value}
            onChange={(ev, option) => onChange(option.key as unknown as number)}
            onRenderOption={(option) => <>{option.data.hierarchicalName}</>}
            options={options}/>
    )
}
