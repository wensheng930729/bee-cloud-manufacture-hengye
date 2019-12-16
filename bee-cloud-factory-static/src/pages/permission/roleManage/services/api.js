import { api_user_prefix } from '@/constants/prefix';
export default {
  getList: {
    api: ({ currentPage, pageSize, roleName }) => `${api_user_prefix}/auth/roles?currentPage=${currentPage}&pageSize=${pageSize}${roleName !== undefined ? `&roleName=${roleName}` : ''}`, //查询列表
    type: 'GET'
  },
  deleteRole: {
    api: (id) => `${api_user_prefix}/auth/role/${id}`,
    type: 'DELETE'
  },
  getDetail: {
    api: (id) => `${api_user_prefix}/auth/role/${id}`,
    type: 'GET'
  },
  save: {
    api: () => `${api_user_prefix}/auth/role`, //保存角色信息，
    type: `POST`
  },
  getResources: {
    api: () => `${api_user_prefix}/auth/resources`,
    type: `GET`
  },
}