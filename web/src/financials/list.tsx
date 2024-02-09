import * as React from 'react';
import {PropsWithChildren, useCallback, useEffect, useMemo, useState} from 'react';
import {
  CheckboxVisibility,
  DetailsList,
  DetailsListLayoutMode,
  IColumn, IconButton,
  Label,
  SearchBox,
  SelectionMode,
  Stack,
  TextField
} from '@fluentui/react';
import {currencies, currencyFields, currencySymbols} from './currencies';
import styled from '@emotion/styled';
import {useDebouncedInput} from '../common/debounced-input.hook';
import {collapseISODate, expandISODate, isoDateToFullDisplay} from '../common/date.utils';
import {BasicISODate, Currency, Money} from '@pim/common';
import {CurrencyInput} from './currency-input';
import {stackTokens} from './styles';
import {cancelIcon, hideIcon} from '../notes/icons';

interface ListProps<T = unknown> {
  data: T[];
  sortConfig?: SortConfig;
  sortData?: (sort: SortConfig) => void;
  idField?: string;
  searchableTextFields?: string[];
  onClick?: (row: T) => void;
  columnsSelector?: (columnName: string) => boolean;
}

const idFields = ['id', 'categoryId', 'recordId'];

const fieldIsNotId = fieldName => !idFields.includes(fieldName)
const formatFieldName = fieldName => {
  if (currencies.includes(fieldName.toUpperCase())) {
    return fieldName.toUpperCase();
  }

  const spaces = fieldName
    .split('_').join(' ') //underscores to spaces
    .replace(/([a-z])([A-Z])/g, '$1 $2'); //camel case to spaces
  return spaces.split(' ')
    .map(word =>
      currencies.includes(word.toUpperCase()) ?
	word.toUpperCase() :
	word.charAt(0).toUpperCase() + word.slice(1)
    )
    .join(' ');
}

// Counts characters in a displayed number
// Note: doesn't include the displayed thousands separators. In any case the font
// isn't monospace so this just gets an estimate of the displayed width
const countDigits = value =>
  Math.floor(Math.log10(Math.abs(value) < 1 ? 1 : Math.abs(value))) + 1 + //Leading zeroes are displayed when values are between -1 and 1
    (value < 0 ? 1 : 0) + //the minus sign
    3; //decimal point plus the two digits after the decimal

const CurrencyField = ({value, currencyCode, maxDigits}) => {
  if (!value && value !== 0) return null;
  return (
    <StyledCurrency>
      {/* This is a hack and not pixel-perfect. Better solution would be to use the maxDigits to set the
      column width, and then use justify-content: space-between; on StyledCurrency */}
      <span style={{position: 'absolute', marginRight: (maxDigits * 6) + 10}}>
	{currencySymbols[currencyCode]}
      </span>
      <span>{value.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 })}</span>
    </StyledCurrency>
  );
}

const StyledCurrency = styled.span`
  display: flex;
  justify-content: flex-end;
`

function isDateColumn(key: string) {
  return key.toLowerCase().includes('date') || key.toLowerCase() === 'timestamp';
}

function getColumnRenderer(key: string, data) {
  let maxSize = 1;
  if (currencyFields.has(key)) {
    data.forEach(row => {
      maxSize = Math.max(maxSize, countDigits(row[key]))
    })
  }
  
  const columnRenderer = (value) => isDateColumn(key) ?
    isoDateToFullDisplay(value[key]) :
    currencyFields.has(key) ?
      <CurrencyField value={value[key]} currencyCode={currencyFields.get(key)} maxDigits={maxSize}/> :
      value[key];
  return columnRenderer; 
}

interface DateColumnFilters {
  start?: BasicISODate;
  end?: BasicISODate
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
  columnsSelector,
  onClick}: PropsWithChildren<ListProps<T>>): JSX.Element {

    const [filterMenuColumn, setFilterMenuColumn] = useState<IColumn>(null)
    const [columnFilters, setColumnFilters] = useState<ColumnFilters>({})

    const columns = useMemo(() => {
      if (!data.length) return [];

      const keys = Object.keys(data[0]);
      return keys.filter(key => columnsSelector ? columnsSelector(key) : fieldIsNotId(key)).map(key => ({
        key,
        isFiltered: !!columnFilters[key],
        minWidth: currencies.includes(key.toUpperCase()) ? 100 : 150,
        maxWidth: currencies.includes(key.toUpperCase()) ? 100 : null,
        fieldName: key,
        onRender: getColumnRenderer(key, data),
        name: formatFieldName(key),
        styles: currencyFields.has(key) ? { //style the header
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
    }, [data, sortConfig, sortData, columnFilters, columnsSelector])

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

const CurrencyFilterMenu: React.FC<FilterMenuProps> = ({column, columnFilters, setColumnFilters}) => {
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

  return (
    <>
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
    </>
  )
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

  let filterInputs;
  if (isDateColumn(column.key)) {
    const filter = columnFilters[column.key] as DateColumnFilters;
    filterInputs = (
      <>
        <TextField label="Start"
          type='Date'
          value={filter?.start ? expandISODate(filter?.start) : undefined}
          onChange={(_, value) => {
            setColumnFilters(prev => ({
              ...prev,
              [column.key]: {
                ...prev[column.key],
                start: collapseISODate(value) as number
              }
            }))
          }} />
        <TextField label="End"
          type='Date'
          value={filter?.end ? expandISODate(filter?.end) : undefined}
          onChange={(_, value) => {
            setColumnFilters(prev => ({
              ...prev,
              [column.key]: {
                ...prev[column.key],
                end: collapseISODate(value) as number
              }
            }))
          }} />
      </>
    )
  } else if (currencyFields.has(column.key)) {
    filterInputs = <CurrencyFilterMenu {...props} />
  }

  return filterInputs ? (
    <Stack styles={{root: {paddingTop: 10, width: 'fit-content'}}}>
      <Stack horizontal tokens={stackTokens}>
        <Label styles={{root: {flexGrow: 1}}}>{column.name}</Label>
        <IconButton iconProps={hideIcon} onClick={() => setFilterMenuColumn(null)}/>
        <IconButton iconProps={cancelIcon} onClick={() => {
          setColumnFilters(prev => { const curr = {...prev}; delete curr[column.key]; return curr; })
          setFilterMenuColumn(null)
        }}/>
      </Stack>
      <Stack horizontal tokens={stackTokens}>
        {filterInputs}
      </Stack>
    </Stack>
  ) : null;
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
      const dateValue = row[key];
      if (filter.start && dateValue < filter.start) {
        return false
      }
      if (filter.end && dateValue > filter.end) {
        return false
      }
    }

    if (currencyFields.has(key)) {
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
