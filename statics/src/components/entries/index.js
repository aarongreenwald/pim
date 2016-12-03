import React, { Component } from 'react'
import moment from 'moment'

const EntriesItem = ({startTimestamp, content, openEntry}) => 
  <div style={styles.entriesItem}> 
    <span className="timestamp">{moment(startTimestamp).format('ll')}</span>
    <span style={{...styles.buttons, float: 'right'}} onClick={openEntry}>View</span>
    <div style={styles.content} dangerouslySetInnerHTML={{__html: content.replace(/\n/g, '<br />')}}></div>    
  </div>


const Entries = ({entries, openEntry, newEntry}) => 
  <div>
    <h1>Diary</h1>
    <span style={styles.buttons} onClick={newEntry}>New</span>
  {
    entries.map((e, i) => 
      <EntriesItem key={i} {...e}
        openEntry={() => openEntry(e)}
      />
    )
  }
  </div>



const styles = {
  entriesItem: {
    border: '1px solid black',
    margin: 5,
    padding: 5
  },
  content: {
    marginTop: 10
  },
  buttons: {  
    color: 'blue',
    cursor: 'pointer'
    
  }
}

export default Entries