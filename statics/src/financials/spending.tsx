import {useCallback, useState} from 'react';
import * as React from 'react';

export default function Spending() {
  const [item, setItem] = useState({
    paidDate: new Date().getTime()
  })
  const updateItem = useCallback(({target}) => {
    setItem({
      ...item,
      [target.name]: target.value
    })
  }, [item])

  return (
    <div>
      <form>
        <div>
          <label>Date</label>
          <input type="date" onChange={updateItem} value={item.paidDate} name="paidDate"/>
        </div>
        <div>
          <label>Incurred Start Date</label>
          <input type="date" onChange={updateItem}/>
        </div>
        <div>
          <label>Incurred Start Date</label>
          <input type="date" onChange={updateItem}/>
        </div>
        <div>
          <label>Incurred End Date</label>
          <input type="date" onChange={updateItem}/>
        </div>
        <div>
          <label>Counterparty</label>
          <input type="text" onChange={updateItem}/>
        </div>
        <div>
          <label>Amount</label>
          <input type="number" onChange={updateItem}/>
        </div>
        <div>
          <label>Currency</label>
          <input type="text" onChange={updateItem}/>
        </div>
      </form>
    </div>
  )
}