import {currencySymbols} from '../currencies';
import * as React from 'react';
import {FuelLog, PaymentId} from '@pim/common';
import {FontIcon, Link} from '@fluentui/react'
import styled from '@emotion/styled';
import {formatDay, formatTime, isMidnight} from '../../common/date.utils';

//TODO finish extracting the inline styles to styled component,s reuse the margin/spacing sizes and colors
export const FuelLogCard: React.FC<FuelLogCardProps> = ({fuelLog, onViewPayment}) => {

    return (
        <StyledCard>
            <div style={{borderBottom: '1px solid #ccc', marginBottom: 4}}>
                <span>
                    {formatDay(fuelLog.timestamp)}
                </span>
                <span style={{marginLeft: 4, fontSize: 'smaller'}}>
                    {!isMidnight(fuelLog.timestamp) && formatTime(fuelLog.timestamp)}
                </span>
            </div>
            <div style={{display: 'flex', justifyContent: 'space-between'}}>
                <StyledCardColumn>
                    <span style={{fontSize: 'x-large', marginRight: 4, alignSelf: 'center'}}>{fuelLog.kilometersPerLiter?.toFixed(2)}</span>
                    <span style={{alignSelf: 'flex-end', fontSize: 'small'}}>km/l</span>
                </StyledCardColumn>
                <StyledCardColumn>
                    <CardValue value={fuelLog.odometer} iconName={'Clock'} suffix={'km'} />
                    <CardValue value={fuelLog.liters} iconName={'DropShapeSolid'} suffix={'l'} />
                </StyledCardColumn>
                <div>
                    <CardValue value={fuelLog.kilometers} suffix={'km'} />
                    <CardValue value={fuelLog.totalCost.toFixed(2)} suffix={currencySymbols[fuelLog.currency.toLowerCase()]} onClick={onViewPayment} />
                </div>
            </div>
            <StyledNote>{fuelLog.note}</StyledNote>
        </StyledCard>
    )
}

const StyledCard = styled.div`
  max-width: 400px; //TODO instead of this, render a DetailsList for large screens and cards in small screens
  border: 1px solid #cccccc;
  padding: 8px;
  margin-top: 4px;
  box-shadow: 0 1px 12px 1px #ccc;
`

const StyledCardColumn = styled.div`
  padding-right: 8px;
  border-right: 1px solid #cccccc;
`

const StyledCardValue = styled.div`
  text-align: right;
`

const StyledSmaller = styled.span`
  font-size: smaller;
`

const StyledNote = styled(StyledSmaller)`
  font-style: italic;
`

const CardValue = ({value, iconName = null, suffix = null, onClick = null}) => (
    <StyledCardValue>
        {
            iconName && <StyledSmaller><FontIcon iconName={iconName} /> </StyledSmaller>
        }
        {
            onClick ?
                <Link onClick={onClick}>{value}</Link> :
                value
        }
        {
            suffix && <StyledSmaller> {suffix}</StyledSmaller>
        }
    </StyledCardValue>
)

interface FuelLogCardProps {
    fuelLog: FuelLog;
    onViewPayment: (paymentId: PaymentId) => void;
}