import {PanelProps} from '../../common/panel.types';
import * as React from 'react';
import {useEffect, useState} from 'react';
import {CashAssetAllocationHistory} from '@pim/common';
import {getCashAssetAllocationHistory} from '../../services/server-api';
import {List} from '../list';

export const CashAllocationHistory: React.FC<PanelProps<string>> = ({id}) => {
  const [allocationHistory, setAllocationHistory] = useState<CashAssetAllocationHistory[]>(null)
  useEffect(() => {
    getCashAssetAllocationHistory(id).then(setAllocationHistory);
  }, [id])


  return (
    allocationHistory &&
      <List<CashAssetAllocationHistory>
        data={allocationHistory} />
  )
}
