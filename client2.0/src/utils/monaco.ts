import { MonacoLanguageClient, MessageConnection, CloseAction, ErrorAction, MonacoServices, createConnection } from 'monaco-languageclient';
import { listen } from '@codingame/monaco-jsonrpc';
import normalizeUrl from 'normalize-url';
import { buildWorkerDefinition } from "monaco-editor-workers";

const HASKELL_LANGUAGE_ID = "haskell"

type Monaco = typeof import("monaco-editor/esm/vs/editor/editor.api");

function createUrl(hostname: string, port: number, path: string): string {
  const protocol = window.location.protocol === 'https:' ? 'wss' : 'ws';
  return normalizeUrl(`${protocol}://${hostname}:${port}${path}`);
}

function createLanguageClient(connection: MessageConnection): MonacoLanguageClient {
  return new MonacoLanguageClient({
      name: "Haskell Language Client",
      clientOptions: {
          // use a language id as a document selector
          documentSelector: [HASKELL_LANGUAGE_ID],
          // disable the default error handler
          errorHandler: {
              error: () => ErrorAction.Continue,
              closed: () => CloseAction.DoNotRestart
          }
      },
      // create a language client connection from the JSON RPC connection on demand
      connectionProvider: {
          get: (errorHandler, closeHandler) => {
              return Promise.resolve(createConnection(connection, errorHandler, closeHandler))
          }
      }
  });
}

export const registerMonaco = (monaco: Monaco) => {
  buildWorkerDefinition('../../node_modules/monaco-editor-workers/dist/workers', import.meta.url, false);

  // register Monaco languages
  monaco.languages.register({
    id: HASKELL_LANGUAGE_ID,
    extensions: ['.hs'],
    aliases: ['haskell'],
    mimetypes: ['application/json'],
  });

  // install Monaco language client services
  MonacoServices.install(monaco);

  // create the web socket
  const url = createUrl('localhost', 3003, '/')
  const webSocket = new WebSocket(url);

  // listen when the web socket is opened
  listen({
    webSocket,
    onConnection: connection => {
      // create and start the language client
      const languageClient = createLanguageClient(connection);
      const disposable = languageClient.start();
      connection.onClose(() => disposable.dispose());

      console.log(connection);

      console.log(`Connected to "${url}" and started the language client.`);
    }
  });
};
