'use strict';
import * as ws from "ws";
import * as http from "http";
import * as url from "url";
import * as net from "net";

import express from "express";
import * as path from 'path';
import { TextDocuments } from 'vscode-languageserver/lib/node/main';
import {
  ExecutableOptions,
  LanguageClient,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/lib/node/main';
import {
  LanguageClientOptions,
  RevealOutputChannelOn,
} from 'vscode-languageclient/lib/common/client';

// import { DocsBrowser } from 'haskell/docsBrowser';
import { URI } from 'vscode-uri';

import * as TextDocumentImpl from "vscode-languageserver-textdocument";


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
      // launch the HLS when the web socket is opened
      if (webSocket.readyState === webSocket.OPEN) HLSProxy.launch(socket);
      else webSocket.on('open', () => HLSProxy.launch(socket));
    });
  }
})

// The current map of documents & folders to language servers.
// It may be null to indicate that we are in the process of launching a server,
// in which case don't try to launch another one for that uri
const clients: Map<string, LanguageClient | null> = new Map();

// This is the entrypoint to our extension
/*
export async function activate() {
  // (Possibly) launch the language server every time a document is opened, so
  // it works across multiple workspace folders. Eventually, haskell-lsp should
  // just support
  // https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#workspace_workspaceFolders
  // and then we can just launch one server
  workspace.onDidOpenTextDocument(async (document: TextDocument) => await activateServerForDocument(document));
  workspace.textDocuments.forEach(async (document: TextDocument) => await activateServerForDocument(document));

  // Set up the documentation browser.
  const docsDisposable = DocsBrowser.registerDocsBrowser();
  context.subscriptions.push(docsDisposable);

  const openOnHackageDisposable = DocsBrowser.registerDocsOpenOnHackage();
  context.subscriptions.push(openOnHackageDisposable);
}
*/

function HLSLanguageClientFactory(socket: net.Socket) {
  if (!socket.localPort) throw `Socket is missing a local port!`;

  const serverExecutable: string = '/root/.ghcup/bin/haskell-language-server-wrapper-1.6.1.0';
  const args: string[] = ['--lsp'];

  const exeOptions: ExecutableOptions = {
    cwd: path.dirname(__dirname),
    env: { ...process.env },
  };

  const serverOptions: ServerOptions = {
    run: { command: serverExecutable, transport: { kind: TransportKind.socket, port: socket.localPort }, args, options: exeOptions },
    debug: { command: serverExecutable, transport: { kind: TransportKind.socket, port: socket.localPort }, args, options: exeOptions },
  };

  const clientOptions: LanguageClientOptions = {
    // Use the document selector to only notify the LSP on files inside the folder
    // path for the specific workspace.
    documentSelector: [
      { scheme: 'file', language: 'haskell' },
      { scheme: 'file', language: 'literate haskell' },
    ],
  
    synchronize: {
      // Synchronize the setting section 'haskell' to the server.
      configurationSection: 'haskell',
    },
    revealOutputChannelOn: RevealOutputChannelOn.Never,
    middleware: {
      // provideHover: DocsBrowser.hoverLinksMiddlewareHook,
      // provideCompletionItem: DocsBrowser.completionLinksMiddlewareHook,
    },
    // Launch the server in the directory of the workspace folder.
    // workspaceFolder: './',
  };

  // Create the LSP client.
  const langClient = new LanguageClient('haskell', 'haskell', serverOptions, clientOptions);

  // Register ClientCapabilities for stuff like window/progress
  langClient.registerProposedFeatures();

  return langClient;
}

/*
 * Deactivate each of the LSP servers.
 */
export async function deactivate() {
  const promises: Thenable<void>[] = [];
  for (const client of clients.values()) {
    if (client) {
      promises.push(client.stop());
    }
  }
  await Promise.all(promises);
}


class HLSProxy {
  protected workspaceRoot: URI | undefined;
  protected readonly documents = new TextDocuments(TextDocumentImpl.TextDocument);
  protected readonly pendingValidationRequests = new Map<string, NodeJS.Timeout>();

  constructor(
    protected readonly languageClient: LanguageClient
  ) {
    this.languageClient = languageClient;
    this.languageClient.start();
  }

  public static launch(socket: net.Socket) {
    const languageClient = HLSLanguageClientFactory(socket);
    return new HLSProxy(languageClient);

  }
}