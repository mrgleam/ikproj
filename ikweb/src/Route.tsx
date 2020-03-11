import React, { useEffect, useState, useContext, useRef } from 'react'
import { Router, Switch, Route } from 'react-router'
import { createBrowserHistory } from 'history'
import { Link } from 'react-router-dom'
import * as api from './api'

const history = createBrowserHistory()
const LoginHandler = ({ history }: any) => {
  const [email, setEmail] = useState('')
  const [password, setPassword] = useState('')
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState('')

  const handleSubmit = async (e: any) => {
    e.preventDefault()
    setLoading(true)
    const l = await api.login(email, password)
    setLoading(false)
    if (l.error) {
      setError(l.error)
      history.push('/login')
    } else {
      history.push('/')
    }
  }

  if (loading) {
    return <h4>Logging in...</h4>
  }

  return (
    <div style={{ marginTop: '1rem' }}>
      <form onSubmit={handleSubmit}>
        <input
          type="email"
          placeholder="Enter email address"
          value={email}
          onChange={e => setEmail(e.target.value)}
        />
        <input
          type="password"
          placeholder="Enter password"
          value={password}
          onChange={e => setPassword(e.target.value)}
        />
        <input type="submit" value="Login" />
      </form>
      {error && <h2>{error}</h2>}
    </div>
  )
}

const LogoutHandler = ({ history }: any) => {
  useEffect(() => {
    history.push('/login')
  }, [history])
  return <div>Logging out!</div>
}

const ProtectedHandler = ({ history }: any) => {
  // const session = useContext(SessionContext);
  useEffect(() => {
    api.session().then(s => {
      if (s.error) {
        history.push('/login')
      }
    })
  }, [history])

  return (
    <div>
      <h6>Protected data for </h6>
      <Link to="/logout">Logout here</Link>
    </div>
  )
}
export const Routes = () => {
  return (
    <Router history={history}>
      <div className="navbar">
        <h6 style={{ display: 'inline' }}>Nav Bar</h6>
      </div>
      <Switch>
        <Route path="/login" component={LoginHandler} />
        <Route path="/logout" component={LogoutHandler} />
        <Route path="*" component={ProtectedHandler} />
      </Switch>
    </Router>
  )
}
