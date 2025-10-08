# Player Data Structure

## Thư mục: `../data/players/`

Mỗi player sẽ có một file JSON riêng với tên file là `{player_id}.json`

## Cấu trúc file JSON:

```json
{
  "id": "player_xxxxxx",
  "name": "string",
  "avatar": "string"
}
```

## Chi tiết các trường:

### `id` (string)
- Format: `player_` + 6 ký tự ngẫu nhiên
- 6 ký tự bao gồm: a-z, A-Z, 0-9 (không phân biệt hoa thường)
- Ví dụ: `player_A7x9K2`, `player_m3N8q1`

### `name` (string)

- Biệt danh người chơi nhập vào
- Validate: 2-30 ký tự, chỉ chứa a-z, A-Z, 0-9, _, -
- Ví dụ: `Admiral_Blue`, `Sea_Wolf_123`

### `avatar` (string)

- Đường dẫn đầy đủ đến file avatar
- Giá trị có thể: `"../assets/images/captain.jpg"`, `"../assets/images/daden.jpg"`, `"../assets/images/female.jpg"`
- Lưu path để dễ load từ nguồn
