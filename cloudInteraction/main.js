const WebSocket = require('ws')
const File = require('node:fs')
const path = require('path')
const crypto = require('crypto')
const base64 = require('base64-xor');
const https = require('https')
const md5 = require('md5')

const Encrypt = require("./encrypt.js")
const Encryption = new Encrypt()

const privateKey = File.readFileSync('./ssl-cert/privkey.pem', 'utf8');
const certificate = File.readFileSync('./ssl-cert/fullchain.pem', 'utf8');

const credentials = { key: privateKey, cert: certificate };

const httpsServer = https.createServer(credentials);
httpsServer.listen(443);

const WebSocketServer = require('ws').Server;
const wss = new WebSocketServer({
    server: httpsServer
});

const Cipher_code = '6a 75 73 74 73 69 6d 70 6c 65 74 65 78 74'

const usersList = new Map()
const steamData = new Map()

const obj_to_string = function (obj) {
  let str = '';
  for (let p in obj) {
      if (obj.hasOwnProperty(p)) {
          str += p + obj[p];
      }
  }
  return str;
}

const create_signature = function(j) {
  return md5(obj_to_string(j) + 'secretOPDFAKSpo')
}

const signObject = function(object) {
  const signature = create_signature(object)
  object['signature'] = signature

  return object
}

const sleep = ms => new Promise(r => setTimeout(r, ms));

const relayMessage = function(object) {
  return base64.encode(Cipher_code, JSON.stringify(object))
}

const relayUserInformation = async function(instance) {
  let data = {  }

  wss.clients.forEach(async (client) => {
    if (client.readyState === WebSocket.OPEN) {
      const ip = (client._socket.remoteAddress).replace('::ffff:', '')

      if (usersList.get(ip)) {
        const userInstance = usersList.get(ip).instance
        const username = usersList.get(ip).username

        await Array.from(steamData.values()).reduce((acc, value) => {
          if (value.instance === instance & userInstance === instance & username == value.username) {
            data[username] = value.xuid
            return acc + 1;
          }
          return acc;
        }, 0)

        const message = { type: 'heartbeat', method: 'usersXuid', instance: instance, data: JSON.stringify(data) }

        if (userInstance === instance) {
          client.send(relayMessage(message))
        }
      }
    }
  })
}

const relayActiveConnectionsHeartbeat = async function(instance) {
  await sleep(100)
  wss.clients.forEach((client) => {
    if (client.readyState === WebSocket.OPEN) {
      const activeConnections = Array.from(usersList.values()).reduce((acc, value) => {
        if (value.instance === instance) {
          return acc + 1;
        }
        return acc;
      }, 0)

      const ip = (client._socket.remoteAddress).replace('::ffff:', '')
      const message = { type: 'heartbeat', method: 'updateOnlineUsers', activeConnections }

      if (usersList.get(ip)) {
        const userInstance = usersList.get(ip).instance

        if (userInstance === instance) {
          client.send(relayMessage(message))
        }
      }
    }
  })
}

const relayCloudHeartbeat = function(instance) {
  wss.clients.forEach(async (client) => {
    if (client.readyState === WebSocket.OPEN) {
      const directoryPath = `./cloud/${instance}`
      const outputObject = {  }
      await File.readdir(directoryPath, async (err, files) => {
        try {
          if (err) {
            console.error('Error reading directory:', err);
            return;
          }

          const jsonFiles = files.filter(file => path.extname(file) === '.json');

          jsonFiles.forEach(file => {
            const filePath = path.join(directoryPath, file);
            const fileContent = File.readFileSync(filePath, 'utf8');

            const deserialized = JSON.parse(fileContent)

            outputObject[file.replace('.json', '')] = {
              'configName': deserialized.configName,
              'configData': deserialized.configData,
              'uploadTimestamp': deserialized.uploadTimestamp,
            }
          })

          const message = {
            'type': 'heartbeat',
            'method': 'cloudUpdate',
            'instance': instance,
            'configs': JSON.stringify(outputObject)
          }

          const ip = (client._socket.remoteAddress).replace('::ffff:', '')
          if (usersList.get(ip)) {
            const userInstance = usersList.get(ip).instance

            if (userInstance === instance) {
              client.send(relayMessage(message))
            }
          }
        }

        catch (e) {
          console.log(e)
        }
      })
    }
  })
}

const sortAndConcat = function(obj) {
  const keys = Object.keys(obj).sort();
  let result = "";
  for (let i = 0; i < keys.length; i++) {
    const key = keys[i];
    result += `${key}${obj[key]}`;
  }
  return result;
}

const buildSign = function(obj) {
  const signature = sortAndConcat(obj) + 'secretOPDFAKSpo'

  return md5(signature)
}

wss.on('connection', (ws, request) => {
    try {
      let username
      const clientAddress = (request.socket.remoteAddress).replace('::ffff:', '')
      ws.on('message', async (buffer) => {
        try {
          const text = buffer.toString()
          const unXored = base64.decode(Cipher_code, text)

          const unhash = JSON.parse(unXored)

          if (unhash.method === 'validateNewConnection') {
            username = unhash.username

            const ruleSign = create_signature({
              username: username,
              method: 'validateNewConnection',
              instance: unhash.instance,
              key: unhash.key,
            })

            if (ruleSign !== unhash.signature) {
              console.log(`Rejected new request -> ${username} as ${unhash.instance} | got: ${unhash.signature} | valid: ${ruleSign}`)

              return
            }

            console.log(`Registered new request -> ${username} as ${unhash.instance}`)
            usersList.set(clientAddress, {
                username: unhash.username,
                instance: unhash.instance,
            })

            ws.send(relayMessage({
              type: 'heartbeat',
              method: 'authorizeResponse',
              success: true
            }))

            relayActiveConnectionsHeartbeat(unhash.instance)
            return
          }

          if (!unhash.tickcount > 0 || !usersList.get(clientAddress)) {
            console.log('secure error')

            return
          }

          if (unhash.method === 'xuidUpdate') {
            const xuid = unhash.xuid

            if (!xuid) {
              steamData.delete(clientAddress)
            } else {
              steamData.set(clientAddress, {
                username: unhash.username,
                xuid: xuid,
                instance: unhash.instance
              })
            }

            await relayUserInformation(unhash.instance)

            return
          }

          const without_sign = await Object.assign({}, unhash); delete without_sign['signature']
          const sign = buildSign(await without_sign)
          console.log(`username [${unhash.username}] | instance [${unhash.instance}] | server [${sign}] | client [${unhash.signature}] | v [${unhash.signature === sign}]`)

          if (sign !== unhash.signature) {
            return
          }
            

          if (unhash.cloud) {
            const instance = unhash.instance
            const configName = unhash.configName
            const configData = unhash.configData

            const targetFile = `./cloud/${instance}/${username}.json`

            switch (unhash.method) {
              case 'insert':

                File.writeFile(targetFile, JSON.stringify({
                  'configName': configName,
                  'configData': configData,
                  'uploadTimestamp': unhash.timestamp,
                }), function() {
                  relayCloudHeartbeat(instance)
                })
                break;

              case 'remove':
                  File.unlink(targetFile, function() {
                    relayCloudHeartbeat(instance)
                  })
                break;

              case 'update':
                relayCloudHeartbeat(instance)
                break;

              case 'user_info':
                relayUserInformation(instance)
              break;
            }
          }
        }

        catch (e) {
          console.log(e)
        }
      })

      ws.on('close', async (test) => {
        try {
          if (usersList.get(clientAddress)) {
            const instance = usersList.get(clientAddress).instance

            usersList.delete(clientAddress)
            steamData.delete(clientAddress)

            await relayUserInformation(instance)
            await relayActiveConnectionsHeartbeat(instance)
          }
        }

        catch (e) {
          console.log(e)
        }
      })
    }

    catch (e) {
      console.log(e)
      //ws.close(402, 'Если бы эта была ошибка - то я бы расстроился, иди нахуй долбаеб')
      console.log('error')
    }
})
