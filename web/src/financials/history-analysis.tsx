import {useEffect, useState} from 'react';
import {CategoryId, SpendingByCategory} from '@pim/common';
import {CategoryDropdown} from './category-dropdown';
import {getSpendingByCategory} from '../services/server-api';
import {List} from './list';
import * as React from 'react';

export const HistoryAnalysis: React.FC = () => {
    const [selectedCategory, setSelectedCategory] = useState<CategoryId>(null)
    const [byCategoryData, setByCategoryData ] = useState<SpendingByCategory[]>(null);

    useEffect(() => {
        if (selectedCategory) {
            getSpendingByCategory(selectedCategory).then(setByCategoryData)
        }

    }, [selectedCategory])
    return (
        <>
            <CategoryDropdown value={selectedCategory} onChange={setSelectedCategory} />
            {
                byCategoryData &&
                    <List
                        data={byCategoryData}
                        idField={'id'}
                    />
            }
        </>
    )
}