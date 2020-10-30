import {List} from './table';
import {useEffect, useState} from 'react';
import * as React from 'react';
import {getIncome} from '../services/server-api';

export const Income: React.FC = () => {
    const [income, setIncome] = useState([]);
    useEffect(() => {getIncome().then(setIncome)}, [])

    return (
        <List data={income} idField={'income_id'} />
    )
}