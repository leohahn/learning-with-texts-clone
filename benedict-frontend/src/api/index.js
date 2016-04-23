export function login (body) {
  const options = {
    method: 'POST',
    headers: new Headers({
      'Content-Type': 'application/json'
    }),
    body: JSON.stringify(body)
  }
  return fetch('http://localhost:8081/api/login', options)
}
