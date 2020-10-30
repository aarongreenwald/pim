import {useEffect, useState} from 'react';
import {getCarSummary} from '../services/server-api';
import {List} from './table';
import * as React from 'react';
import {CarSummary} from '@pim/common';

export const CashRecordHistory: React.FC = () => {
    const [carSummary, setCarSummary] = useState<CarSummary[]>([]);
    useEffect(() => {getCarSummary().then(setCarSummary)}, [])

    return (
        <List data={carSummary} idField={'recordDate'} />
    )
}