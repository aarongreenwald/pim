import * as React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {Payment} from '@pim/common';
import {getPayments} from '../services/server-api';
import {DataGrid} from '@material-ui/data-grid';
import {DetailsList, DetailsListLayoutMode, SelectionMode} from '@fluentui/react';

export const Payments: React.FC = () => {

    const [payments, setPayments] = useState<Payment[]>()
    useEffect(() => {
        getPayments().then(setPayments)
    }, [])

    const sortPayments = useCallback((sort: Sort) => {
        const newPayments = [...payments];
        const sortFn =
            sort.direction ?
                (a, b) => a[sort.fieldName] > b[sort.fieldName] ? 1 : -1 :
                (a, b) => a[sort.fieldName] < b[sort.fieldName] ? 1 : -1

        newPayments.sort(sortFn)

        setPayments(newPayments)
    }, [payments])

    return (
        payments ? <List data={payments} sortData={sortPayments} /> : null
    )
}

interface TableProps {
    data: any[];
    sortData?: (sort: Sort) => void;
}

export const HtmlTable: React.FC<TableProps> = ({data}) => {
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

interface Sort {
    fieldName: string;
    direction: number; //TODO name these
}

const defaultSortDirection = (fieldName: string): number => 0

export const List: React.FC<TableProps> = ({data, sortData}) => {
    const [sort, setSort] = useState<Sort>(null)
    const columns = useMemo(() => {
        const keys = Object.keys(data[0]);
        return keys.map(key => ({
            key,
            minWidth: 100,
            fieldName: key,
            name: key,
            isResizable: true,
            isSorted: sort?.fieldName === key,
            isSortedDescending: sort?.direction === 0,
            // isCollapsible: true, //what does this do?
            onColumnClick: () => {
                const newSort = {
                    fieldName: key,
                    direction: sort?.fieldName === key ? ~sort?.direction : defaultSortDirection(key)
                };
                setSort(newSort)
                sortData(newSort);
            }
        }))
    }, [data, sort])

    const items = useMemo(() => data.map(item => ({...item, id: item.payment_id})), [data])

    const getKey = useCallback((item) => item.id, []);

    return <DetailsList items={items}
               getKey={getKey}
               compact
               layoutMode={DetailsListLayoutMode.fixedColumns}
               selectionMode={SelectionMode.none}
               columns={columns}/>
}

export const Grid: React.FC<TableProps> = ({data}) => {
    const columns = useMemo(() => {
        const keys = Object.keys(data[0]);
        return keys.map(key => ({
            field: key,
            type: key.indexOf('date') ? 'date' : 'string',
            headerName: key
        }))
    }, [data])

    const foo = useMemo(() => data.map(item => ({...item, id: item.payment_id})), [data])

    return (
        <DataGrid rows={foo} columns={columns} />
    )
}