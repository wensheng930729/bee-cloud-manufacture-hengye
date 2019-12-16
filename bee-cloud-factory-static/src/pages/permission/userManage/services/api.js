import { api_user_prefix } from '@/constants/prefix';
export default {
  getRoles: {
    api: () => `${api_user_prefix}/auth/roles?pageSize=100`, //获取角色列表
    type: 'GET'
  },
  getList: {
    api: ({ currentPage, pageSize, keyword, roleId }) => `${api_user_prefix}/platform/auth/users?currentPage=${currentPage}&pageSize=${pageSize}${keyword !== undefined ? `&keyword=${keyword}` : ''}${roleId !== undefined ? `&roleId=${roleId}` : ''}`, //查询列表
    type: 'GET'
  },
  change: {
    api: (id) => `${api_user_prefix}/platform/auth/user/${id}`,
    type: 'PUT'
  },
  save: {
    api: () => `${api_user_prefix}/platform/auth/user`, //新增客户，
    type: `POST`
  },
  update: {
    api: () => `${api_user_prefix}/platform/auth/user`, //编辑客户，
    type: 'PUT'
  }
}