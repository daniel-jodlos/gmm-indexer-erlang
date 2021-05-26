# Rest API offered by our server

---

## Define parameters

| Parameter | Type | Format | Represents |
|-----------|------|--------|------------|
| from, to, of, id | string | "zone_i/characters" | ID of a vertex |
| type, name | string |  | Some value |
| permissions | string | "xx-x-" | Permissions set on given edge |
| trace | string |  | Globally unique identifier of set of events about the same operation on the graph |
| successive | boolean |  | I have no idea, but Kamil has it |

---

## Manipulating Vertices

### Create new vertex

<table>
    <tr>
        <th>Path</th>
        <td>/graph/vertices</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>type: string</li>
            <li>name: string</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>JSON: {"id": id_of_new_vertex}</td>
    </tr>
</table>

### Get all vertices

<table>
    <tr>
        <th>Path</th>
        <td>/graph/vertices</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>GET</td>
    </tr>
    <tr>
        <th>Params</th>
        <td>none</td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>JSON: map( type -> list(string) )<br/>Where type: &lt;"user", "group", "space", "provider"&gt;</td>
    </tr>
</table>

### Get details of vertex by ID

<table>
    <tr>
        <th>Path</th>
        <td>/graph/vertices</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>GET</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>id: string</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>JSON: map( field -> string )<br/>Where field: &lt;"id", "type", "name", "zone"&gt;</td>
    </tr>
</table>

### Delete vertex

<table>
    <tr>
        <th>Path</th>
        <td>/graph/vertices</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>DELETE</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>id: string</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

---

## Manipulating Edges

### Create edge

<table>
    <tr>
        <th>Path</th>
        <td>/graph/edges</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>from: string</li>
            <li>to: string</li>
            <li>permissions: string</li>
            <li>trace: string, optional</li>
            <li>successive: boolean</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

### Set permissions

<table>
    <tr>
        <th>Path</th>
        <td>/graph/edges/permissions</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>from: string</li>
            <li>to: string</li>
            <li>permissions: string</li>
            <li>trace: string, optional</li>
            <li>successive: boolean</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

### Delete edge

<table>
    <tr>
        <th>Path</th>
        <td>/graph/edges/delete</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>from: string</li>
            <li>to: string</li>
            <li>trace: string, optional</li>
            <li>successive: boolean</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>void</td>
    </tr>
</table>

---

## Basic queries about structure of the graph (edges)

### Check if edge exists

<table>
    <tr>
        <th>Path</th>
        <td>/is_adjacent</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>from: string</li>
            <li>to: string</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>boolean</td>
    </tr>
</table>

### Get permissions of the edge

<table>
    <tr>
        <th>Path</th>
        <td>/permissions</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>from: string</li>
            <li>to: string</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>string</td>
    </tr>
</table>

### Get children of given vertex

<table>
    <tr>
        <th>Path</th>
        <td>/list_adjacent</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>of: string</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>list(string)</td>
    </tr>
</table>

### Get parents of given vertex

<table>
    <tr>
        <th>Path</th>
        <td>/list_adjacent_reversed</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>of: string</li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td>void</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>list(string)</td>
    </tr>
</table>

---

## Empty table template

<table>
    <tr>
        <th>Path</th>
        <td></td>
    </tr>
    <tr>
        <th>Method</th>
        <td></td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li></li>
        </ul></td>
    </tr>
    <tr>
        <th>Body</th>
        <td></td>
    </tr>
    <tr>
        <th>Return type</th>
        <td></td>
    </tr>
</table>
