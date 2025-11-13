ChatBox Đa Người Dùng (Multi-User Chat Application)

Giới thiệu

Đây là một ứng dụng Chatbox đa người dùng đơn giản nhưng mạnh mẽ, được xây dựng hoàn toàn bằng ngôn ngữ lập trình hàm Haskell. 
Dự án được thiết kế theo kiến trúc Client-Server sử dụng TCP Sockets và áp dụng các kỹ thuật lập trình đồng thời (Concurrency), 
đặc biệt là STM (Software Transactional Memory) để quản lý trạng thái Server an toàn.

Công nghệ Sử dụng

- Ngôn ngữ: Haskell
- Giao thức: TCP Socket (Network.Socket)
- Đồng bộ hóa: STM (Software Transactional Memory) với TVar (Control.Concurrent.STM)
- Đồng thời: Luồng nhẹ (Lightweight Threads) với forkFinally (Control.Concurrent)

Các Tính năng Nổi bật

- Kiến trúc Client-Server: Thiết lập kênh truyền tin cậy qua TCP Socket.
- Xử lý Đồng thời: Server sử dụng một luồng riêng biệt (forkFinally) cho mỗi Client kết nối, đảm bảo khả năng xử lý nhiều người dùng cùng lúc.
- Đồng bộ hóa An toàn: Sử dụng TVar để quản lý danh sách người dùng (ClientMap). Mọi thao tác thêm, xóa người dùng đều được bọc trong giao dịch atomically, loại bỏ nguy cơ Race Condition và đảm bảo tính nguyên tử (Atomic) của các thay đổi trạng thái.
- Quản lý Tài nguyên Tinh gọn: Hàm forkFinally đảm bảo tài nguyên Socket (Handle) luôn được đóng và người dùng luôn được xóa khỏi danh sách (cleanup) ngay cả khi kết nối bị ngắt đột ngột (I/O Errors).
- Thông báo Trạng thái: Tự động thông báo ra toàn bộ phòng chat khi có người dùng tham gia (<Tên> joined the chat) hoặc rời đi.
