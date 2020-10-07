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
            sort.direction === SortDirection.asc ?
                (a, b) => a[sort.fieldName] > b[sort.fieldName] ? 1 : -1 :
                (a, b) => a[sort.fieldName] < b[sort.fieldName] ? 1 : -1

        newPayments.sort(sortFn)

        setPayments(newPayments)
    }, [payments])

    return (
        payments ? <List data={payments}
                         sortData={sortPayments}
                         idField={'payment_id'} /> : null
    )
}

// eslint-disable-next-line @typescript-eslint/ban-types
interface TableProps<T extends object = {}> {
    data: T[];
    sortData?: (sort: Sort) => void;
    idField: string;
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
    direction: SortDirection;
}

enum SortDirection {
    asc,
    desc
}

const defaultSortDirection = (fieldName: string): number => fieldName.includes('date') ? SortDirection.desc : SortDirection.asc

export const List: React.FC<TableProps> = ({data, sortData, idField}) => {
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
            isSortedDescending: sort?.direction === SortDirection.desc,
            // isCollapsible: true, //what does this do?
            onColumnClick: () => {
                const newSort = {
                    fieldName: key,
                    direction: sort?.fieldName === key ?
                        Number(!sort?.direction) :
                        defaultSortDirection(key)
                };
                setSort(newSort)
                sortData(newSort);
            }
        }))
    }, [data, sort, sortData])

    const items = useMemo(() => data.map(item => ({...item, id: item[idField]})), [data, idField])

    const getKey = useCallback((item) => item.id, []);

    return <DetailsList items={items}
               getKey={getKey}
               compact
               layoutMode={DetailsListLayoutMode.fixedColumns}
               selectionMode={SelectionMode.none}
               columns={columns}/>
}

export const Grid: React.FC<TableProps> = ({data, idField}) => {
    const columns = useMemo(() => {
        const keys = Object.keys(data[0]);
        return keys.map(key => ({
            field: key,
            type: key.includes('date') ? 'date' : 'string',
            headerName: key
        }))
    }, [data])

    const rows = useMemo(() => data.map(item => ({...item, id: item[idField]})), [data, idField])

    return (
        <DataGrid rows={rows} columns={columns} />
    )
}