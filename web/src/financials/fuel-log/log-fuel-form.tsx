import {PanelProps} from '../../common/panel.types';
import {DefaultButton, Label, PrimaryButton, Stack, TextField} from '@fluentui/react';
import {stackTokens} from '../styles';
import * as React from 'react';
import {useCallback, useState, useEffect} from 'react';
import {FuelLog, NewFuelLogDto, DropdownItemDto, LatestFuelLogsDto} from '@pim/common';
import {saveFuelLog, getVehicles, getLatestFuelLogs} from '../../services/server-api';
import {Dropdown} from '../../common/dropdown';
import {CurrencyInput} from '../currency-input';
import {FuelLogPreviewCard} from './fuel-log-preview-card';
import {todayAsISODate} from '../../common/date.utils';

export const LogFuelForm: React.FC<PanelProps<number, FuelLog>> = ({onClose, onSave}) => {

  const {fuelLog, updateFuelLog, updateVehicle, submitForm} = useLogFuelForm(onClose, onSave);
  const [vehicles, setVehicles] = useState<DropdownItemDto[]>()
  const [latestFuelLogs, setLatestFuelLogs] = useState<LatestFuelLogsDto>()
  
  useEffect(() => {
    getVehicles().then(setVehicles)
  }, [])

  useEffect(() => {
    getLatestFuelLogs().then(setLatestFuelLogs)
  }, [])


  const previousFuelLog = latestFuelLogs && fuelLog ? latestFuelLogs[fuelLog.vehicleId] : null;
  const km = Math.max(fuelLog.odometer - previousFuelLog?.odometer, 0);
  const kml = fuelLog.liters ? km / fuelLog.liters : null;
  const totalCost = fuelLog.liters * fuelLog.price;

  return (
    <form>
      <Stack tokens={stackTokens}>

        {/*<TextField*/}
        {/*    label={'Date'}*/}
        {/*    type="datetime-local"*/}
        {/*    onChange={updateFuelLog}*/}
        {/*    value={fuelLog.timestamp?.toString()}*/}
        {/*    name="timestamp"/>*/}

        <Dropdown
          items={vehicles}
          onChange={updateVehicle}
          label="Vehicle"
          value={fuelLog.vehicleId}
        />

        <TextField
          label="Odometer"
          value={fuelLog.odometer ? fuelLog.odometer.toString() : ''}
          type="number"
          name="odometer"
          onChange={updateFuelLog}
        />

        <TextField
          label="Liters"
          value={fuelLog.liters ? fuelLog.liters.toString() : ''}
          type="number"
          name="liters"
          onChange={updateFuelLog}
        />

        <CurrencyInput
          amount={fuelLog.price}
          label="Price"
          name="price"
          onChange={updateFuelLog}
        />

        <TextField
          label="Notes"
          name="note"
          value={fuelLog.note}
          multiline
          onChange={updateFuelLog}/>

        <Label>Preview</Label>
        <FuelLogPreviewCard
          kilometersPerLiter={kml}
          kilometers={km}
          totalCost={totalCost} />

        <Stack horizontal tokens={stackTokens}>
          <PrimaryButton onClick={submitForm}>Save</PrimaryButton>
          <DefaultButton onClick={onClose}>Cancel</DefaultButton>
        </Stack>

      </Stack>
    </form>
  )
}


function useLogFuelForm(onClose: () => void, onSave: () => void) {
  const [fuelLog, setFuelLog] = useState<NewFuelLogDto>(initializeFuelLog())

  const updateFuelLog = useCallback(({target}) => {
    setFuelLog({
      ...fuelLog,
      [target.name]: target.value
    })
  }, [fuelLog])

  const updateVehicle = useCallback((id: number) => {
    setFuelLog({
      ...fuelLog,
      vehicleId: id
    })
  }, [fuelLog]);


  const submitForm = useCallback(async () => {
    await saveFuelLog(fuelLog)
    onSave()
    onClose()
  }, [fuelLog, onClose, onSave])

  return {fuelLog, updateFuelLog, updateVehicle, submitForm};
}

function initializeFuelLog(): NewFuelLogDto {
  return {
    id: -1,
    date: todayAsISODate() as number, // currently only for payment
    timestamp: new Date(), // for fuel log - TODO use date + local time as separate fields
    price: null, //TODO set to previously used price
    odometer: 0,
    liters: 0,
    note: '',
    isFull: true,
    vehicleId: null
  };
}
