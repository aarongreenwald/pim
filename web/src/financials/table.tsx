import * as React from 'react';
import {useCallback, useMemo} from 'react';
import {DetailsList, DetailsListLayoutMode, SelectionMode} from '@fluentui/react';
// import {DataGrid} from '@material-ui/data-grid';

// eslint-disable-next-line @typescript-eslint/ban-types
interface TableProps<T extends object = {}> {
    data: T[];
    sortConfig?: SortConfig;
    sortData?: (sort: SortConfig) => void;
    idField: string;
    onClick?: (row: T) => void;
}

/*
Naive implementation
 */
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

/*
Table based on fluent DetailsList
 */
export const List: React.FC<TableProps> = ({
                                               data,
                                               sortConfig,
                                               sortData,
                                               idField,
                                               onClick
                                           }) => {

    const columns = useMemo(() => {
        const keys = Object.keys(data[0]);
        return keys.filter(key => key === 'id' || !key.endsWith('_id')).map(key => ({
            key,
            minWidth: 100,
            fieldName: key,
            onRender: (value) => key.toLowerCase().includes('date') ? new Date(value[key]).toDateString() : value[key],
            name: key,
            isResizable: true,
            isSorted: sortConfig?.fieldName === key,
            isSortedDescending: sortConfig?.direction === SortDirection.desc,
            // isCollapsible: true, //what does this do?
            onColumnClick: () => {
                const newSort = {
                    fieldName: key,
                    direction: sortConfig?.fieldName === key ?
                        Number(!sortConfig?.direction) :
                        defaultSortDirection(key)
                };
                sortData(newSort);
            }
        }))
    }, [data, sortConfig, sortData])

    const items = useMemo(() => data.map(item => ({...item, id: item[idField]})), [data, idField])

    const getKey = useCallback((item) => item.id, []);

    return (
        <DetailsList
            items={items}
            columns={columns}
            getKey={getKey}
            compact
            onActiveItemChanged={onClick}
            layoutMode={DetailsListLayoutMode.fixedColumns}
            selectionMode={onClick ? SelectionMode.single : SelectionMode.none}/>
    )
}

/*
Table based on MUI DataGrid
 */
// export const Grid: React.FC<TableProps> = ({data, idField}) => {
//     const columns = useMemo(() => {
//         const keys = Object.keys(data[0]);
//         return keys.map(key => ({
//             field: key,
//             type: key.includes('date') ? 'date' : 'string',
//             headerName: key
//         }))
//     }, [data])
//
//     const rows = useMemo(() => data.map(item => ({...item, id: item[idField]})), [data, idField])
//
//     return (
//         <DataGrid rows={rows} columns={columns} />
//     )
// }

const defaultSortDirection = (fieldName: string): number => fieldName.includes('date') ? SortDirection.desc : SortDirection.asc

export interface SortConfig {
    fieldName: string;
    direction: SortDirection;
}

export enum SortDirection {
    asc,
    desc
}