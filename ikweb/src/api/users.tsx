import axios from 'axios'
import { resolve } from './resolve'

export const getProfile = async () => {
  return await resolve(axios.get('/profile', { withCredentials: true }).then(res => res.data))
}
