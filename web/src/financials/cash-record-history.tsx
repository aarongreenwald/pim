import {useEffect, useState} from 'react';
import {getCarSummary} from '../services/server-api';
import {List} from './table';
import * as React from 'react';

export const CashRecordHistory: React.FC = () => {
    const [carSummary, setCarSummary] = useState([]);
    useEffect(() => {getCarSummary().then(setCarSummary)}, [])

    return (
        <List data={carSummary} idField={'record_date'} />
    )
}