/**
 * Sends a login POST request to the api. The method must receive
 * a JSON Object with keys 'username' and 'password'.
 * @param  {String} options.username Username of the account.
 * @param  {String} options.password Password of the account.
 * @return {Promise}                 Returns a Promise of the Fetch method.
 */
export function login ({ username, password }) {
  const options = {
    method: 'POST',
    headers: new Headers({
      'Content-Type': 'application/json'
    }),
    body: JSON.stringify({
      username,
      password
    })
  }
  return fetch('http://localhost:8081/api/login', options)
}
