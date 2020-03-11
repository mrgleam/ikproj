import axios from 'axios'
import { resolve } from './resolve'

export const login = async (username: string, password: string) => {
  return await resolve(
    axios.post('/login', { username, password }, { withCredentials: true }).then(res => res.data)
  )
}

export const session = async () => {
  return await resolve(axios.get('/session', { withCredentials: true }).then(res => res.data))
}
