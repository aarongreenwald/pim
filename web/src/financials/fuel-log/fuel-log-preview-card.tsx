import {currencySymbols, defaultCurrency} from '../currencies';
import * as React from 'react';

//TODO reuse styles/code from the FuelLogCard
export const FuelLogPreviewCard: React.FC<FuelLogCardProps> = ({kilometersPerLiter, kilometers, totalCost}) => {
  return (
    <div style={{display: 'flex', justifyContent: 'space-between', border: '1px solid gray', padding: 8, boxShadow: '0px 1px 14px 1px rgba(125,125,125,0.94)'}}>
      <div style={{minWidth:100, paddingRight: 10, borderRight: '1px solid gray'}}>
        <span style={{fontSize: 'x-large', marginRight: 5, alignSelf: 'center'}}>{kilometersPerLiter?.toFixed(2) || '--.--'}</span>
        <span style={{alignSelf: 'flex-end', fontSize: 'small'}}>km/l</span>
      </div>
      <div>
        <div style={{textAlign: 'right'}}>
          {kilometers} <span style={{fontSize: 'smaller'}}>km</span>
        </div>
        <div style={{textAlign: 'right'}}>
          {totalCost.toFixed(2)}<span style={{fontSize: 'smaller'}}> {currencySymbols[defaultCurrency.toLowerCase()]}</span>
        </div>
      </div>
    </div>

  )
}

interface FuelLogCardProps {
  kilometersPerLiter: number;
  kilometers: number;
  totalCost: number;
}
