import {useEffect, useState} from 'react';
import {Payment} from '@pim/common';
import {getPayments} from '../services/server-api';
import * as React from 'react';

export const Payments: React.FC = () => {

    const [payments, setPayments] = useState<Payment[]>()
    useEffect(() => {
        getPayments().then(setPayments)
    }, [])

    return (
        <FinancialsTable data={payments} />
    )
}

interface TableProps {
    data: any[];
}

export const FinancialsTable: React.FC<TableProps> = ({data}) => {
    if (!data?.length) {
        return null;
    }
    const keys = Object.keys(data[0])
    return (
        <table>
            <thead>

                {keys.map(key =>
                    <td key={key}>
                        {key}
                    </td>
                )}

            </thead>
            <tbody>
            {
                data.map((row, i) => {
                    return (
                        <tr key={i}>
                            {keys.map(key =>
                                <td key={key}>
                                    {row[key]}
                                </td>
                            )}
                        </tr>
                    )
                })
            }
            </tbody>
        </table>
    )
}