import * as React from 'react';
import {PropsWithChildren, useCallback, useEffect, useMemo, useState} from 'react';
import {
    CheckboxVisibility,
    DefaultButton,
    DetailsList,
    DetailsListLayoutMode,
    IColumn,
    Label,
    SearchBox,
    SelectionMode,
    Stack,
    TextField
} from '@fluentui/react';
import {currencyFields, currencySymbols} from './currencies';
import styled from '@emotion/styled';
import {useDebouncedInput} from '../common/debounced-input.hook';
import {formatDay} from '../common/date.utils';
import {Currency, Money} from '@pim/common';
import {CurrencyInput} from './currency-input';

interface ListProps<T = unknown> {
    data: T[];
    sortConfig?: SortConfig;
    sortData?: (sort: SortConfig) => void;
    idField: string;
    searchableTextFields?: string[];
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

function isDateColumn(key: string) {
    return key.toLowerCase().includes('date') || key.toLowerCase() === 'timestamp';
}

function getColumnRenderer(key: string) {
    return (value) => isDateColumn(key) ? //TODO - consider putting the time portion in a tooltip or perhaps as part of the string
        new Date(value[key]).toDateString() :
        currencyFields.includes(key) ?
            <Currency value={value[key]} currencyCode={key}/> :
            value[key];
}

interface DateColumnFilters {
    start?: Date;
    end?: Date
}

interface CurrencyColumnFilters {
    min?: Money;
    max?: Money;
}
interface ColumnFilters {
    [columnKey: string]: DateColumnFilters | CurrencyColumnFilters
}

export function List<T = unknown>({data,
                                   sortConfig,
                                   sortData,
                                   idField,
                                   searchableTextFields,
                                   onClick}: PropsWithChildren<ListProps<T>>): JSX.Element {

    const [filterMenuColumn, setFilterMenuColumn] = useState<IColumn>(null)
    const [columnFilters, setColumnFilters] = useState<ColumnFilters>({})

    const columns = useMemo(() => {
        if (!data.length) return [];

        const keys = Object.keys(data[0]);
        return keys.filter(fieldIsNotId).map(key => ({
            key,
            isFiltered: !!columnFilters[key],
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
            onColumnContextMenu: (column?: IColumn) => setFilterMenuColumn(column),
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
    }, [data, sortConfig, sortData, columnFilters])

    const {inputVal, debouncedValue, updateValue} = useDebouncedInput('');
    const filteredData = useMemo(() =>
        debouncedValue || Object.keys(columnFilters).length ?
            filterRows(data, debouncedValue, searchableTextFields, columnFilters) :
            data,
        [data, debouncedValue, columnFilters, searchableTextFields])

    const getKey = useCallback((item) => item[idField], [idField]);
    return (
        <>
            {
                searchableTextFields &&
                <SearchBox
                    styles={searchStyles}
                    underlined
                    placeholder={'Search'}
                    onChange={val => updateValue(val.target.value)}
                    onClear={() => updateValue('')}
                    value={inputVal}/>
            }
            {
                filterMenuColumn &&
                <FilterMenu
                    column={filterMenuColumn}
                    columnFilters={columnFilters}
                    setColumnFilters={setColumnFilters}
                    setFilterMenuColumn={setFilterMenuColumn}/>
            }
            {
                !!filteredData.length &&
                <DetailsList
                    checkboxVisibility={CheckboxVisibility.hidden}
                    items={filteredData}
                    columns={columns}
                    getKey={getKey}
                    compact
                    onActiveItemChanged={onClick}
                    layoutMode={DetailsListLayoutMode.fixedColumns}
                    selectionMode={onClick ? SelectionMode.single : SelectionMode.none}/>
            }
        </>
    )
}

const CurrencyFilterMenu: React.FC<FilterMenuProps> = ({column, columnFilters, setColumnFilters, setFilterMenuColumn}) => {
    const filter = columnFilters[column.key] as CurrencyColumnFilters;
    const {inputVal: minInputVal, debouncedValue: debouncedMinValue, updateValue: updateMinValue} = useDebouncedInput<number>(filter?.min);
    useEffect(() => {
        setColumnFilters(prev => ({
            ...prev,
            [column.key]: {
                ...prev[column.key],
                min: debouncedMinValue
            }
        }))
    }, [column, debouncedMinValue, setColumnFilters])

    const {inputVal: maxInputVal, debouncedValue: debouncedMaxValue, updateValue: updateMaxValue} = useDebouncedInput<number>(filter?.max);
    useEffect(() => {
        setColumnFilters(prev => ({
            ...prev,
            [column.key]: {
                ...prev[column.key],
                max: debouncedMaxValue
            }
        }))
    }, [debouncedMaxValue, column, setColumnFilters])

    return <Stack styles={{root: {maxWidth: 200}}}>
        <Label>{column.name}</Label>
        <CurrencyInput
            label="Minimum"
            amount={minInputVal}
            currency={column.key as Currency}
            onChange={(_, value) => updateMinValue(value)}
        />
        <CurrencyInput
            label="Maximum"
            amount={maxInputVal}
            currency={column.key as Currency}
            onChange={(_, value) => updateMaxValue(value)}
        />
        <DefaultButton onClick={() => {
            setColumnFilters(prev => {const curr = {...prev}; delete curr[column.key]; return curr;})
            setFilterMenuColumn(null)
        }}>Clear</DefaultButton>
    </Stack>
}

type SetStateFunc<T> = (value: (((prevState: T) => T) | T)) => void

interface FilterMenuProps {
    column: IColumn;
    columnFilters: ColumnFilters;
    setColumnFilters: SetStateFunc<ColumnFilters>;
    setFilterMenuColumn: SetStateFunc<IColumn>;
}

const FilterMenu: React.FC<FilterMenuProps> = (props) => {
    const {column, columnFilters, setColumnFilters, setFilterMenuColumn} = props;

    if (isDateColumn(column.key)) {
        const filter = columnFilters[column.key] as DateColumnFilters;
        return <Stack styles={{root: {maxWidth: 400}}}>
            <Label>{column.name}</Label>
            <TextField label="Start"
                       type='Date'
                       value={filter?.start ? formatDay(filter?.start) : undefined}
                       onChange={(_, value) => {
                           setColumnFilters(prev => ({
                               ...prev,
                               [column.key]: {
                                   ...prev[column.key],
                                   start: new Date(value)
                               }
                           }))
                       }} />
            <TextField label="End"
                       type='Date'
                       value={filter?.end ? formatDay(filter?.end) : undefined}
                       onChange={(_, value) => {
                           setColumnFilters(prev => ({
                               ...prev,
                               [column.key]: {
                                   ...prev[column.key],
                                   end: new Date(value)
                               }
                           }))
                       }} />
            <DefaultButton onClick={() => {
                setColumnFilters(prev => {const curr = {...prev}; delete curr[column.key]; return curr;})
                setFilterMenuColumn(null)
            }}>Clear</DefaultButton>
        </Stack>
    }

    if (currencyFields.includes(column.key)) {
        return <CurrencyFilterMenu {...props} />
    }

    return null;
}

function filterRows<T>(data: T[], filterText: string, textFields: string[], filters: ColumnFilters) {
    const searchStr = filterText.trim().toLowerCase();
    return data.filter(row => {
        return textFields.some(key => row[key]?.toLowerCase().includes(searchStr)) &&
            rowMatchesColumnFilters(row, filters)
    });
}

function rowMatchesColumnFilters<T>(row: T, filters: ColumnFilters): boolean {
    for (const key in filters) {
        if (isDateColumn(key)) {
            const filter = filters[key] as DateColumnFilters;
            const dateValue = new Date(row[key]);
            if (filter.start && dateValue < filter.start) {
                return false
            }
            if (filter.end && dateValue > filter.end) {
                return false
            }
        }

        if (currencyFields.includes(key)) {
            const filter = filters[key] as CurrencyColumnFilters;
            const amount = row[key];
            if (filter.min?.toString() !== '' && amount < filter.min) {
                return false
            }
            if (filter.max?.toString() !== '' &&amount > filter.max) {
                return false
            }
        }
    }
    return true;
}

const defaultSortDirection = (fieldName: string): number =>
    fieldName.toLowerCase().includes('date') ?
        SortDirection.desc :
        SortDirection.asc;

const searchStyles = {root: {maxWidth: 300}};

export interface SortConfig {
    fieldName: string;
    direction: SortDirection;
}

export enum SortDirection {
    asc,
    desc
}