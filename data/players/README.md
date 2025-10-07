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

## Flow hoạt động:

1. User nhập nickname và chọn avatar ở `initial.html`
2. Click "BẮT ĐẦU" → validate input
3. Generate unique `player_id` (6 ký tự random)
4. Tạo object JSON với structure trên
5. Lưu file JSON vào `../data/players/{player_id}.json`
6. Lưu session data vào localStorage
7. Redirect đến `home.html`

## Ví dụ file tạo ra:

**File:** `../data/players/player_A7x9K2.json`

```json
{
  "id": "player_A7x9K2",
  "name": "Admiral_Blue", 
  "avatar": "../assets/images/captain.jpg"
}
```

## Notes

- Mỗi player có file JSON riêng biệt
- File name = player ID để dễ tìm kiếm
- Cấu trúc đơn giản, chỉ 3 trường: id, name, avatar
- Avatar lưu đường dẫn đầy đủ để dễ load
- Không có timestamps - data hoàn toàn ephemeral
- Tất cả data bị xóa khi F5 hoặc close browser
- Mỗi lần mở trang = session hoàn toàn mới