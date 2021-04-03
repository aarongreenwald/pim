import {PanelProps} from '../common/panel.types';
import {DefaultButton, Label, PrimaryButton, Stack, TextField} from '@fluentui/react';
import {stackTokens} from './styles';
import * as React from 'react';
import {useCallback, useState} from 'react';
import {FuelLog, NewFuelLogDto} from '@pim/common';
import {saveFuelLog} from '../services/server-api';
import {CurrencyInput} from './currency-input';
import {FuelLogPreviewCard} from './fuel-log-preview-card';

export const LogFuelForm: React.FC<PanelProps<number, FuelLog>> = ({onClose, onSave, data}) => {
    const previousFuelLog = data;
    const {fuelLog, updateFuelLog, submitForm} = useLogFuelForm(onClose, onSave);
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

    const submitForm = useCallback(async () => {
        await saveFuelLog(fuelLog)
        onSave()
        onClose()
    }, [fuelLog, onClose, onSave])

    return {fuelLog, updateFuelLog, submitForm};
}

function initializeFuelLog(): NewFuelLogDto {
    return {
        id: -1,
        timestamp: new Date(),
        price: null, //TODO set to previously used price
        odometer: 0,
        liters: 0,
        note: '',
        isFull: true
    };
}