export const resolve = async (promise: any) => {
  const resolved: { data: any; error: string | null } = {
    data: null,
    error: null,
  }

  try {
    resolved.data = await promise
  } catch (e) {
    if (e.config && e.config.url) {
      if (e.response.status === 500) {
        resolved.error = 'Internal Server Error'
      } else {
        resolved.error = `Network ${e.toString()} ${e.response?.data?.toString()}`
      }
    } else {
      resolved.error = e.toString()
    }
  }

  return resolved
}
