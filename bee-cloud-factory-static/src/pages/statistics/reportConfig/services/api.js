import { api_factory_prefix } from '@/constants/prefix';
export default {
  getList: {
    api: ({ currentPage, pageSize, name }) => `${api_factory_prefix}/configReportForms/searchReportFormsList?currentPage=${currentPage}&pageSize=${pageSize}${name ? `&name=${name}` : ''}`,
    type: 'GET'
  },
  enable: {
    api: () => `${api_factory_prefix}'/configReportForms/enableReportForms`,
    type: 'POST'
  },
  disable: {
    api: () => `${api_factory_prefix}/configReportForms/stopReportForms`,
    type: 'POST'
  }
}
