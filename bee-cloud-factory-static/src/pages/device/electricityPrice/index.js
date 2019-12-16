import { Component } from 'react'
import { Button, Card, Table, Divider, message, Popconfirm } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../components/BreadCrumb'
import router from 'umi/router'
import {
  searchElectricityPriceList,
  deleteElectricityPriceById
} from '../services'
import * as utils from '@/utils/utils'

const elTypes = ['炉变电', '动力电']

@withRouter
export default class Index extends Component {
  state = {
    data: [],
    currentPage: 1,
    pageSize: 10,
    totalPage: 0,
    totalRecords: 0
  }

  componentDidMount() {
    const { currentPage, pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  getData({ currentPage, pageSize }) {
    let obj = {
      currentPage,
      pageSize,
    };

    const str = utils.queryString(obj)
    searchElectricityPriceList(str).then(res => {
      if (res && res.code === 1 && res.object) {
        this.setState({
          data: res.object,
          ...res.page
        })
      }
    })
  }

  onChange = (currentPage) => {
    const { pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  onShowSizeChange = (currentPage, pageSize) => {
    this.getData({ currentPage, pageSize });
  }

  handleDelete = id => {
    const { currentPage, pageSize } = this.state
    deleteElectricityPriceById(id).then(res => {
      if (res.code === 1) {
        message.success(res.message)
        this.getData({ currentPage, pageSize });
      } else {
        message.error(res.message)
      }
    })
  }

  render() {
    const { data, currentPage, pageSize, totalPage, totalRecords } = this.state
    const columns = [
      {
        title: '电价类型',
        dataIndex: 'electricityType',
        key: 'electricityType',
        render(h) {
          return elTypes[h]
        }
      }, {
        title: '时间区间',
        dataIndex: 'name',
        key: 'name',
        render(h, row) {
          return (row.startTime || '00:00') + '~' + row.endTime
        }
      }, {
        title: '电价(元/度)',
        dataIndex: 'price',
        key: 'price'
      }, {
        title: '生效日期',
        dataIndex: 'effectiveDate',
        key: 'effectiveDate'
      }, {
        title: '失效日期',
        dataIndex: 'expirationDate',
        key: 'expirationDate'
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => router.push(`/device/electricityPrice/edit?id=${row.id}`)} style={{ color: '#1890ff', cursor: 'pointer' }}>编辑</span>
            <Divider type="vertical" />
            <Popconfirm placement="topRight" title={'确认删除该电价配置？'} onConfirm={() => this.handleDelete(row.id)} okText="确认" cancelText="取消">
              <span style={{ color: '#1890ff', cursor: 'pointer' }}>删除</span>
            </Popconfirm>
          </div>
        )
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.push('/device/electricityPrice/edit')}>新增</Button>} />
        <div className={styles.container}>
          <Card title="电价管理" bordered={false}>
            <Table
              rowKey="id"
              columns={columns}
              dataSource={data}
              pagination={{
                showQuickJumper: true,
                showSizeChanger: true,
                defaultCurrent: 1,
                defaultPageSize: 10,
                current: currentPage,
                pageSize: pageSize,
                total: totalRecords,
                onChange: this.onChange.bind(this),
                pageSizeOptions: ['10', '20', '30'],
                showTotal: (total, range) =>
                  `共 ${totalRecords} 条记录 第 ${currentPage} / ${totalPage} 页`,
                onShowSizeChange: this.onShowSizeChange.bind(this)
              }}
            />
          </Card>
        </div>
      </div>
    )
  }
}
