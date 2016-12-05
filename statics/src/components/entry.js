
import React from 'react'
import moment from 'moment'

const Entry = ({startTimestamp, content, closeEntry, updateEntry}) => 
  <div> 
    <span className="timestamp">{moment(startTimestamp).format('ll')}</span>
    <span style={{...styles.buttons, float: 'right'}}  onClick={closeEntry}>Close</span>
    <div style={styles.content} 
      contentEditable="true" 
      dangerouslySetInnerHTML={{__html: content.replace(/\n/g, '<br />')}} 
      onInput={event => updateEntry(event.target.innerText)}
      />    
  </div>

const styles = {
  entriesItem: {
    border: '1px solid black',
    margin: 5,
    padding: 5
  },
  content: {
    marginTop: 10, 
    height: 200
  },
  buttons: {  
    color: 'blue',
    cursor: 'pointer'  
  }
}

export default Entry