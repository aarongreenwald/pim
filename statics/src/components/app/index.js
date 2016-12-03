
import React, { Component } from 'react'
import style from './style.css'
import Entries from '../entries'
import Entry from '../entry'

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
    return e
  }

 _updateEntry(entry, newContent) {
   const newEntry = {...entry, content: newContent}
   this.setState({
     entries: this.state.entries.map(e => e !== entry ? e : newEntry),
     currentEntry: newEntry
   })   
 }

 

  _newEntry() {
    fetch('http://localhost:3000/entries', {method: 'POST'})
      .then(res => res.json())
      .then(newEntry => this._openEntry(newEntry))
      .then(newEntry => this.setState({
        entries: [newEntry, ...this.state.entries]
      }))
      .catch(console.error)
  }

  render() {
    return (
        <div className={style.root}>
            
            { this.state.currentEntry ?

                <Entry {...this.state.currentEntry} closeEntry={this._closeEntry.bind(this)} updateEntry={newContent => this._updateEntry(this.state.currentEntry, newContent)}/>
              : <Entries entries={this.state.entries} openEntry={this._openEntry.bind(this)} newEntry={this._newEntry.bind(this)}/>
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
    color: 'blue',
    cursor: 'pointer'
    
  }
}