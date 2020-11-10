import * as React from 'react';
import {PropsWithChildren, useCallback, useMemo} from 'react';
import {CheckboxVisibility, DetailsList, DetailsListLayoutMode, IColumn, SelectionMode} from '@fluentui/react';
import {currencyFields, currencySymbols} from './currencies';
import styled from '@emotion/styled';

interface ListProps<T = unknown> {
    data: T[];
    sortConfig?: SortConfig;
    sortData?: (sort: SortConfig) => void;
    idField: string;
    onClick?: (row: T) => void;
}


const idFields = ['id', 'categoryId'];

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

const Currency = ({value, currencyCode}) => {
    if (!value && value !== 0) return null;
    return (
        <StyledCurrency>
            <span>{currencySymbols[currencyCode]}</span>
            <span>{value?.toFixed(2)}</span>
        </StyledCurrency>
    );
}

const StyledCurrency = styled.span`
  display: flex;
  justify-content: space-between;
`

function getColumnRenderer(key: string) {
    return (value) => key.toLowerCase().includes('date') ?
        new Date(value[key]).toDateString() :
        currencyFields.includes(key) ?
            <Currency value={value[key]} currencyCode={key}/> :
            value[key];
}

export function List<T = unknown>({data,
                                   sortConfig,
                                   sortData,
                                   idField,
                                   onClick}: PropsWithChildren<ListProps<T>>): JSX.Element {

    const columns = useMemo(() => {
        if (!data.length) return [];

        const keys = Object.keys(data[0]);
        return keys.filter(fieldIsNotId).map(key => ({
            key,
            minWidth: currencyFields.includes(key) ? 75 : 150,
            maxWidth: currencyFields.includes(key) ? 75 : null,
            fieldName: key,
            onRender: getColumnRenderer(key),
            name: formatFieldName(key),
            styles: currencyFields.includes(key) ? { //style the header
                root: {
                    textAlign: 'right',
                    width: '100%',
                },
                cellName: {
                    width: '100%'
                }
            } : null,
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
        } as IColumn))
    }, [data, sortConfig, sortData])


    const getKey = useCallback((item) => item[idField], [idField]);

    return data.length ? (
        <DetailsList
            checkboxVisibility={CheckboxVisibility.hidden}
            items={data}
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