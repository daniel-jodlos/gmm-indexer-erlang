# Rest API offered by our server

---

## Possible parameters' descriptions

| Parameter | Type | Format | Represents |
|-----------|------|--------|------------|
| from, to, of, id | string | zone${i}_characters | ID of a vertex |
| type, name | string |  | Some value |
| permissions | string | xx-x- | Permissions set on given edge |
| trace | string (optional) |  | Globally unique identifier of set of events about the same operation on the graph |
| successive | boolean |  | I have no idea, but Kamil has it |
| enabled | boolean |  | Stores info whether enable or disable some setting |

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
            <li>type</li>
            <li>name</li>
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
            <li>id</li>
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
            <li>id</li>
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
            <li>from</li>
            <li>to</li>
            <li>permissions</li>
            <li>trace: optional</li>
            <li>successive</li>
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
            <li>from</li>
            <li>to</li>
            <li>permissions</li>
            <li>trace: optional</li>
            <li>successive</li>
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
            <li>from</li>
            <li>to</li>
            <li>trace: optional</li>
            <li>successive</li>
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
            <li>from</li>
            <li>to</li>
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
            <li>from</li>
            <li>to</li>
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
            <li>of</li>
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
            <li>of</li>
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

## Meta Info - Server Config

### Health check to determine if server is running

<table>
    <tr>
        <th>Path</th>
        <td>/healthcheck</td>
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
        <td>void</td>
    </tr>
</table>

### Index ready - I don't know what it does

<table>
    <tr>
        <th>Path</th>
        <td>/index_ready</td>
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
        <td>boolean</td>
    </tr>
</table>

### Enable/Disable indexation

<table>
    <tr>
        <th>Path</th>
        <td>/indexation</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>PUT</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>enabled</li>
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

### Enable/Disable instrumentation

<table>
    <tr>
        <th>Path</th>
        <td>/instrumentation</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>PUT</td>
    </tr>
    <tr>
        <th>Params</th>
        <td><ul>
            <li>enabled</li>
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

### Check if instrumentation is enabled

<table>
    <tr>
        <th>Path</th>
        <td>/instrumentation</td>
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
        <td>boolean</td>
    </tr>
</table>

### Dependent Zones - I don't know what it does

<table>
    <tr>
        <th>Path</th>
        <td>/dependent_zones</td>
    </tr>
    <tr>
        <th>Method</th>
        <td>POST</td>
    </tr>
    <tr>
        <th>Params</th>
        <td>none</td>
    </tr>
    <tr>
        <th>Body</th>
        <td>JSON: list(string)</td>
    </tr>
    <tr>
        <th>Return type</th>
        <td>JSON: {"zones": list(string)}</td>
    </tr>
</table>

---

## Empty table template

### Description

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
