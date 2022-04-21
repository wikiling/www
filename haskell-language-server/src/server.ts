/* --------------------------------------------------------------------------------------------
 * Copyright (c) 2018-2022 TypeFox GmbH (http://www.typefox.io). All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
import * as ws from "ws";
import * as http from "http";
import * as url from "url";
import * as net from "net";
import * as rpc from "@codingame/monaco-jsonrpc";
import * as server from "@codingame/monaco-jsonrpc/lib/server";
import * as lsp from "vscode-languageserver";

import express from "express";

process.on('uncaughtException', function (err: any) {
  console.error('Uncaught Exception: ', err.toString());
  if (err.stack) {
    console.error(err.stack);
  }
});

export function launch(socket: rpc.IWebSocket) {
  const reader = new rpc.WebSocketMessageReader(socket);
  const writer = new rpc.WebSocketMessageWriter(socket);

  const socketConnection = server.createConnection(reader, writer, () => socket.dispose());
  const serverConnection = server.createServerProcess('haskell', 'haskell-language-server-8.10.7', ['--lsp', '--debug', '--cwd=/app/files']);

  server.forward(socketConnection, serverConnection, message => {
    console.log(message);

    if (rpc.isRequestMessage(message)) {
      if (message.method === lsp.InitializeRequest.type.method) {
        const initializeParams = message.params as lsp.InitializeParams;
        initializeParams.processId = process.pid;
      }
    }
    return message;
  });
}


const app = express();
app.use(express.static(__dirname));
// start the server
const httpServer = app.listen(3003);
// create the web socket
const wss = new ws.Server({
  noServer: true,
  perMessageDeflate: false
});
httpServer.on('upgrade', (request: http.IncomingMessage, socket: net.Socket, head: Buffer) => {
  console.log(-1);
  const pathname = request.url ? url.parse(request.url).pathname : undefined;
  if (pathname === '/') {
    console.log(0);
    wss.handleUpgrade(request, socket, head, webSocket => {
      const socket: rpc.IWebSocket = {
        send: content => webSocket.send(content, error => {
          if (error) throw error;
        }),
        onMessage: cb => webSocket.on('message', cb),
        onError: cb => webSocket.on('error', cb),
        onClose: cb => webSocket.on('close', cb),
        dispose: () => webSocket.close()
      };
      // launch the HLS when the web socket is opened
      if (webSocket.readyState === webSocket.OPEN) launch(socket);
      else webSocket.on('open', () => launch(socket));
    });
  }
})