import { MonacoLanguageClient, MessageConnection, CloseAction, ErrorAction, MonacoServices, createConnection } from 'monaco-languageclient';
import { listen } from '@codingame/monaco-jsonrpc';
import normalizeUrl from 'normalize-url';

type Monaco = typeof import("monaco-editor/esm/vs/editor/editor.api");

function createUrl(hostname: string, port: number, path: string): string {
  const protocol = window.location.protocol === 'https:' ? 'wss' : 'ws';
  return normalizeUrl(`${protocol}://${hostname}:${port}${path}`);
}

function createLanguageClient(connection: MessageConnection): MonacoLanguageClient {
  return new MonacoLanguageClient({
      name: "Sample Language Client",
      clientOptions: {
          // use a language id as a document selector
          documentSelector: ['json'],
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

  // register Monaco languages
  monaco.languages.register({
    id: 'json',
    extensions: ['.json', '.bowerrc', '.jshintrc', '.jscsrc', '.eslintrc', '.babelrc'],
    aliases: ['JSON', 'json'],
    mimetypes: ['application/json'],
  });

  // create Monaco editor
  const value = `{
    "$schema": "http://json.schemastore.org/coffeelint",
    "line_endings": "unix"
  }`;
  monaco.editor.create(document.getElementById("container")!, {
    model: monaco.editor.createModel(value, 'json', monaco.Uri.parse('inmemory://model.json')),
    glyphMargin: true,
    lightbulb: {
        enabled: true
    }
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

      console.log(`Connected to "${url}" and started the language client.`);
    }
  });
};
