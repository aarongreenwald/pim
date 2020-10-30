import * as React from 'react';
import {useCallback, useMemo} from 'react';
import {CheckboxVisibility, DetailsList, DetailsListLayoutMode, SelectionMode} from '@fluentui/react';

// eslint-disable-next-line @typescript-eslint/ban-types
interface ListProps<T extends object = {}> {
    data: T[];
    sortConfig?: SortConfig;
    sortData?: (sort: SortConfig) => void;
    idField: string;
    onClick?: (row: T) => void;
}


const idFields = ['id', 'categoryId'];
const currencyFields = ['ils', 'usd'];

const fieldIsNotId = fieldName => !idFields.includes(fieldName)
const formatFieldName = fieldName => {
    if (currencyFields.includes(fieldName)) {
        return fieldName.toUpperCase();
    }

    const spaces = fieldName
        .split('_').join(' ') //underscores to spaces
        .replace(/([a-z])([A-Z])/g, '$1 $2'); //camel case to spaces
    return spaces.split(' ').map(word => word.charAt(0).toUpperCase() + word.slice(1)).join(' ');
}

function getColumnRenderer(key: string) {
    return (value) => key.toLowerCase().includes('date') ?
        new Date(value[key]).toDateString() :
        currencyFields.includes(key) ?
            value[key]?.toFixed(2) :
            value[key];
}

export const List: React.FC<ListProps> = ({
                                               data,
                                               sortConfig,
                                               sortData,
                                               idField,
                                               onClick
                                           }) => {

    const columns = useMemo(() => {
        if (!data.length) return [];

        const keys = Object.keys(data[0]);
        return keys.filter(fieldIsNotId).map(key => ({
            key,
            minWidth: 100,
            fieldName: key,
            onRender: getColumnRenderer(key),
            name: formatFieldName(key),
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

    return data.length ? (
        <DetailsList
            checkboxVisibility={CheckboxVisibility.hidden}
            items={items}
            columns={columns}
            getKey={getKey}
            compact
            onActiveItemChanged={onClick}
            layoutMode={DetailsListLayoutMode.fixedColumns}
            selectionMode={onClick ? SelectionMode.single : SelectionMode.none}/>
    ) : null;
}

const defaultSortDirection = (fieldName: string): number =>
    fieldName.toLowerCase().includes('date') ?
        SortDirection.desc :
        SortDirection.asc

export interface SortConfig {
    fieldName: string;
    direction: SortDirection;
}

export enum SortDirection {
    asc,
    desc
}