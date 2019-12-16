import { Component } from 'react'
import { Button, Card, Input, Select, message, Table, Icon, Upload, Tooltip } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../../components/BreadCrumb'
import { updateProductSpec, getProductSpecByProductId } from '../../services'
import router from 'umi/router'

const Option = Select.Option

export default class index extends Component {
  state = {
    data: []
  }

  id = this.props.location.query.id || null

  componentDidMount() {
    getProductSpecByProductId(this.id).then(res => {
      if (res.code === 1) {
        this.setState({
          data: res.object
        })
      }
    })
  }

  handleChange = (e, name, dataIndex) => {
    const { data } = this.state
    if (name === 'qualifiedLine') {
      if (e === 1) {
        data.map(item => {
          item.qualifiedLine = 0
        })
      }
    }
    data[dataIndex][name] = e
    this.setState({
      data
    })
  }

  handleDelete = index => {
    const { data } = this.state
    data.splice(index, 1)
    this.setState({
      data
    })
  }

  handleUp = (item, index) => {
    const { data } = this.state;

    item.sort--;
    data[index - 1].sort++;

    data.splice(index, 1)
    data.splice(index - 1, 0, item)

    this.setState({
      data
    })
  }

  handleAdd = () => {
    const { data } = this.state
    // const id = data.length + 1
    const obj = {
      sort: '',
      specName: '',
      qualifiedLine: 0,
      status: 1
    }
    data.push(obj)
    this.setState({
      data
    })
  }

  handleDown = (item, index) => {
    const { data } = this.state;

    item.sort++;
    data[index + 1].sort--;

    data.splice(index, 1)
    data.splice(index + 1, 0, item)

    this.setState({
      data
    })
  }

  handleSubmit = () => {
    const { data } = this.state
    const params = {}
    params.productSpecList = data.map((item, index) => {
      item.productId = Number(this.id)
      item.sort = index + 1
      return item
    })

    updateProductSpec(params).then(res => {
      console.log(res)
      if (res.code === 1) {
        message.success(res.message)
        router.goBack()
      } else {
        message.error(res.message)
      }
    })
  }

  render() {
    const { data } = this.state
    const self = this
    const columns = [
      {
        title: (
          <span>排序<Tooltip placement="top" title={'排序越高，表示该规格品质越好'}><Icon type="question-circle" /></Tooltip></span>
        ),
        dataIndex: 'sort',
        width: '20%',
      }, {
        title: '规格名称',
        dataIndex: 'specName',
        width: '20%',
        render: (h, row, index) => <Input placeholder="规格名称" value={h} onChange={e => self.handleChange(e.target.value, 'specName', index)} />
      }, {
        title: '合格线',
        dataIndex: 'qualifiedLine',
        width: '20%',
        render: (h, row, index) => (
          <Select value={h} onChange={e => self.handleChange(e, 'qualifiedLine', index)} placeholder="合格线" style={{ width: '100%' }}>
            <Option value={1}>是</Option>
            <Option value={0}>否</Option>
          </Select>
        )
      }, {
        title: '状态',
        dataIndex: 'status',
        width: '20%',
        render: (h, row, index) => (
          <Select value={h} onChange={e => self.handleChange(e, 'status', index)} placeholder="状态" style={{ width: '100%' }}>
            <Option value={1}>启用</Option>
            <Option value={0}>停用</Option>
          </Select>
        )
      }, {
        title: '操作',
        dataIndex: 'action',
        render: (h, row, index) => {
          return (
            <div>
              {
                index === 0 ? '' : (
                  <span style={{ color: '#1890ff', cursor: 'pointer', marginRight: 20 }} onClick={() => self.handleUp(row, index)}><Icon type="arrow-up" />上移</span>
                )
              }
              {
                index === data.length - 1 ? '' : (
                  <span style={{ color: '#1890ff', cursor: 'pointer', marginRight: 20 }} onClick={() => self.handleDown(row, index)}><Icon type="arrow-down" />下移</span>
                )
              }
            </div>
          )
        }
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.goBack()}>返回</Button>} />
        <div className={styles.container}>
          <Card
            title="规格配置"
            bordered={false}
            extra={
              <div className={styles.btnBox}>
                <Button type="primary" onClick={this.handleSubmit}>
                  {this.id ? '修改' : '保存'}
                </Button>
                <Button onClick={() => router.goBack()}>取消</Button>
              </div>
            }
          >
            <Card title="规格（属性，等级，品位）" bordered={false}>
              <Table
                rowKey="id"
                columns={columns}
                dataSource={data}
                pagination={false}
              />
              <div className={styles.btn_add} onClick={this.handleAdd}>添加一行</div>
            </Card>
          </Card>
        </div>
      </div>
    )
  }
}
