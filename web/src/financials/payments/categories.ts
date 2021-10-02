import {useEffect, useState} from 'react';
import {Category} from '@pim/common';
import {getAllCategories} from '../../services/server-api';

export function useCategories(): Category[] {
    const [categories, setCategories] = useState<Category[]>()
    useEffect(() => {
        getAllCategories().then(setCategories)
    }, [])
    return categories;
}