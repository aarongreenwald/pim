import {PanelProps} from '../../common/panel.types';
import * as React from 'react';
import {useEffect, useState, useCallback} from 'react';
import {CashAssetAllocationHistory} from '@pim/common';
import {getCashAssetAllocationHistory} from '../../services/server-api';
import {List} from '../list';

export const CashAllocationHistory: React.FC<PanelProps<string>> = ({id}) => {
  const [allocationHistory, setAllocationHistory] = useState<CashAssetAllocationHistory[]>(null)
  useEffect(() => {
    getCashAssetAllocationHistory(id).then(setAllocationHistory);
  }, [id])

  const columnsSelector = useCallback(columnName => columnName != 'allocationCode', [])

  return (
    allocationHistory &&
      <List<CashAssetAllocationHistory>
	columnsSelector={columnsSelector}
        data={allocationHistory} />
  )
}
