
import React, { Component } from 'react'
import { Link } from 'react-router'
import style from './style.css'
import moment from 'moment'


const Entry = ({startTimestamp, content, closeEntry}) => 
  <div> 
    <span className="timestamp">{moment(startTimestamp).format('ll')}</span>
    <span style={styles.buttons} onClick={closeEntry}>Close</span>
    <div style={styles.content} contentEditable="true" dangerouslySetInnerHTML={{__html: content.replace(/\n/g, '<br />')}}></div>    
  </div>

const Entries = ({entries, openEntry}) => 
  <div>
    <h1>Diary</h1>
    <Link to="/">New</Link>
  {
    entries.map((e, i) => 
      <EntriesItem key={i} {...e}
        openEntry={() => openEntry(e)}
      />
    )
  }
  </div>

const EntriesItem = ({startTimestamp, content, openEntry}) => 
  <div style={styles.entriesItem}> 
    <span className="timestamp">{moment(startTimestamp).format('ll')}</span>
    <span style={styles.buttons} onClick={openEntry}>View</span>
    <div style={styles.content} dangerouslySetInnerHTML={{__html: content.replace(/\n/g, '<br />')}}></div>    
  </div>

export default class App extends Component {

  constructor(props) {
    super(props)
    this.state = {
      entries: []
    }
  }
  
  componentWillMount() {    
    fetch('http://localhost:3000/entries')
      .then(res => res.json())
      .then(entries => this.setState({entries}))
      .catch(console.error)
  }

  _closeEntry() {
    this.setState({
      currentEntry: null
    })
  }

  _openEntry(e) {
    this.setState({
      currentEntry: e
    })
  }

  render() {
    return (
        <div className={style.root}>
            
            { this.state.currentEntry ?

                <Entry {...this.state.currentEntry} closeEntry={this._closeEntry.bind(this)}/>
              : <Entries entries={this.state.entries} openEntry={this._openEntry.bind(this)}/>
            }        
            
                      
        </div>
    )
  }
}

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
    float: 'right',
    color: 'blue',
    cursor: 'pointer'
    
  }
}