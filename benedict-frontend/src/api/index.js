export function login (body) {
  const options = {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(body)
  }
  fetch('http://localhost:8081/api/login', options)
    .then((response) => {
      return response.json()
    })
    .then((json) => {
      console.log(json)
    })
    .catch((error) => {
      console.error(error)
    })
}
